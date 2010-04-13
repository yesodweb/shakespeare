# Synopsis

    {-# LANGUAGE QuasiQuotes #-}
    
    import Text.Hamlet
    import Data.Text (pack)
    
    data Person m = Person
        { name :: m HtmlContent
        , age :: m HtmlContent
        , page :: m PersonUrls
        , isMarried :: m Bool
        , children :: m (Enumerator HtmlContent IO)
        }
    data PersonUrls = Homepage | PersonPage String
    
    renderUrls :: PersonUrls -> String
    renderUrls Homepage = "/"
    renderUrls (PersonPage name) = '/' : name
    
    template :: Person (Hamlet PersonUrls IO) -> Hamlet PersonUrls IO ()
    template = [$hamlet|
    !!!
    %html
        %head
            %title Hamlet Demo
        %body
            %h1 Information on $name$
            %p $name$ is $age$ years old.
            %h2
                $if isMarried
                    Married
                $else
                    Not married
            %ul
                $forall children child
                    %li $child$
            %p
                %a!href=@page@ See the page.
    |]
    
    main :: IO ()
    main = do
        let person = Person
                { name = return $ Unencoded $ pack "Michael"
                , age = return $ Unencoded $ pack "twenty five & a half"
                , page = return $ PersonPage "michael"
                , isMarried = return True
                , children = return $ fromList
                                [ Unencoded $ pack "Adam"
                                , Unencoded $ pack "Ben"
                                , Unencoded $ pack "Chris"
                                ]
                }
        printHamlet renderUrls $ template person

Outputs (new lines added for readability):

    <!DOCTYPE html>
    <html><head><title>Hamlet Demo</title></head><body>
    <h1>Information on Michael</h1>
    <p>Michael is twenty five &amp; a half years old.</p>
    <h2>Married</h2>
    <ul><li>Adam</li><li>Ben</li><li>Chris</li></ul>
    <p><a href="/michael">See the page.</a></p>
    </body></html>

# Overview

Hamlet is a templating system loosely based on Haml syntax. Additionally, it is
a monad to support efficient generation of textual data. Since the former is
more usual use case, I will address that first.

Instead of attempting to be a generic templating system, Hamlet attempts to
excel in a specific niche. In particular, the following design goals are
central:

* We only care about HTML output.

* Templates are fully compiled at compile time. What's more, they are embedded in Haskell files via quasi-quoting.

* Constant memory usage is allowed through the use of enumerators. Looping and output both use an enumerator interface. (Actually, output is not *exactly* an enumerator; see the monadic section below for details.)

* We try to avoid the strings problem by forcing all variables to explicitly state whether they require entity encoding.

* All variables are compile-time checked.

Obviously, this makes Hamlet inappropriate for many contexts; however, if you are generating HTML output from a web application, then this might be for you.

# Syntax

You might want to look at [the Haml site](http://haml-lang.com/) before reading
the rest of this, though it's certainly not required. Coming from a Haskell
background, the whitespace-sensitive layout of Hamlet should feel very
comfortable. Let's kick it off with an example:

    1 %html
    2     %head
    3         %title My Site
    4     %body
    5         %h1 $pageTitle$
    6         #wrapper
    7             Hello World!
    8         %a!href=@homeUrl@ Go home

Some of the basics should be obvious here: indentation dictates nesting. So the head and body tags are inside the html tag. Good.

Now, let's get more nitty-gritty: the percent sign means the tag name. So line 3 gets converted to:

    <title>My Site</title>

Line 6 begins with a pound; this denotes the id. However, you'll note that we haven't specified a tag name. In Hamlet (like haml), the default tag name is div. So lines 6 and 7 together generate:

    <div id="wrapper">Hello World!</div>

I haven't used it in this example, but a period will give a class name. This coincides very nicely with CSS syntax. For other attributes, we use the exclamation point. Here's a few examples of this:

    %span!style=color:red This is red
    %a!href=/some/link/ Some Link
    %input!type=checkbox!checked!name=foo

becomes

    <span style="color:red">This is red</span>
    <a href="http://www.google.com/">Google</a>
    <input type="checkbox" checked name="foo">

Which brings us very nicely to line 8. You'll notice that is says @homeUrl@; content wrapped in an @ refers to a function which returns a URL. Each template is supplied with a function which converts a URL datatype to a String. See below for a more in-depth explanation of this.

Finally, we have line 5. Instead of @, here we use $. Content wrapped in $ is a function which returns an HtmlContent. HtmlContent is simply text that specifies whether it requires entity escaping.

Now that we've seen an overview of the syntax, let's get into the details.

## Doctypes

A special rule is that a line which consists entirely of three exclamation points (!!!) will be converted to a doctype statement. By default, this is <!DOCTYPE html>, though this can be overriden when quasi-quoting the template.

## Line parsing

There are four special words to start a line: $if, $elseif, $else and $forall. See below for an explanation of these. Assuming that one of these words is not present, the following applies:

If a line does *not* begin with a percent sign (%), pound (#) or period (.), it is treated as content. See below for the parsing of content. Otherwise, everything up to the first space to taken as a tag definition. Everything after that first space (if present) is parsed as content and nested within the tag.

## Tag parsing

We break up a string at each delimiter. A delimiter is one of: %#.!

A piece beginning with a percent sign denotes the tag name; beginning with a pound denotes the id attribute; beginning with a period is a class attribute.

A piece beginning with an exclamation mark denotes an arbitrary attribute, parsed as follows: everything up to the first equal sign is the attribute key. If there is no equal sign, it is printed as an empty attribute. For example:

    %input!type=checkbox!checked

becomes

    <input type="checkbox" checked>

If there is an equal sign, everything following it is parsed as content (as per the next section).

## Content parsing

There are three special characters in content: @$^. These must appear in pairs in content. If the pair appears right next to each other, such as @@, then a single character is output. In other words,

    michael@@snoyman.com

becomes

    michael@snoyman.com

Otherwise, any text between the pair of delimiters is treated as a "reference". References are a series of period-separated identifiers, such as:

    family.father.name

Conceptually, this means "get the name value from the father value from the family value." In other words, you can pretend that this is object-oriented. However, it's important to understand how this works under the surface. Since we are dealing with Haskell, we treat these identifiers as functions. For the most part, the above reference will be converted to the following Haskell code:

    family argument >>= father >>= name

where argument is the argument you pass to the template. Notice that everything here is monadic; in particular, each of these functions must work within the Hamlet monad. So a valid set of type signatures for this fragment would be:

    argument :: Household
    family :: Household -> Hamlet url IO Family
    father :: Family -> Hamlet url IO Person
    name :: Person -> Hamlet url IO HtmlContent

There is one exception to the above rule: if a forall binds a value to the first identifier in a chain, that is used instead. foralls will be addressed properly below, but for example's sake:

    %ul
        $forall people person
            %li $person.name$

Here, you do not need a person function to be defined.

Now that we've addressed references, what's the difference between the three delimiters ($@^) mentioned above? The dollar sign ($) outputs HtmlContent; the at sign (@) outputs URLs; and the caret (^) embeds other templates. In other words, they each (respectively) require a type signature of:

    Hamlet url m HtmlContent
    Hamlet url m url
    Hamlet url m ()

## Conditionals

As mentioned above, $if, $elseif and $else are treated specially. As a simple example:

    $if person.employed
        Employed
    $elseif person.retired
        Retired
    $else
        Unemployed

In this case, person.employed and person.retired are parsed as references (see Content Parsing above for explanation). In this case, they must return values of type Bool.

## Loops

    1 %ul#employees
    2     $forall company.employees employee
    3         %li $employee.name$

company.employees is parsed as a reference (see Content Parsing above). Each value is bound to the variable employee and then the inner template (line 3) is called.

For efficiency, we use enumerators for the type of employees. See the synopsis section for a full example.

# Monadic Interface

Most users will be happy to just use the quasi-quoter; however, at a certain point, you have to deal with the resulting Hamlet value. This support can be baked into a framework (like [Yesod](http://www.yesodweb.com/)), but someone has to eventually write *that* support. If you're that somebody, this section is for you.

However, even if you don't want to use the Hamlet syntax and quasi-quoter, the monadic interface itself offers you benefits:

* The enumerator interface provides excellent performance.

* By supporting this interface, you can interoperate easily with Hamlet templates.

* You have built in support for url to String conversions to get great type-safe URLs and work well with web-routes.

* The HtmlContent type helps you solve the string problem in the escaping of HTML entities.

So let's look at the actual datatype of Hamlet:

    type Iteratee val seed m = seed -> val -> m (Either seed seed)

    newtype Hamlet url m a = Hamlet
        { runHamlet :: forall seed.
           (url -> String)
        -> seed
        -> Iteratee Text seed m
        -> m (Either seed (a, seed))
        }

This looks confusing, but it's not too bad. Hamlet is a newtype so that it can define its own Monad instance, and to be existential over the seed value without using deprecated extensions.

There are three type variables: url, m and a. a is simply the contained value, like in any monad. Since Hamlet is a monad transformer, m is the inner monad. This will usually be IO.

url has to do with type-safe URLs. We want to take advantage of the type system of Haskell to ensure we generate only valid URLs, so we create a datatype to represent all possible URLs for an application. That way, any value of type url is *guaranteed* to correlate to a valid URL.

Which brings us nicely to the first argument: we can't print out arbitrary data types to HTML documents; we need a String. The first argument (url -> String) provides this conversion.

The next two arguments form the enumerator interface for outputting the HTML document. If you're not familiar with enumerators, I recommend looking at the WAI documentation, which discusses this form of enumerators fairly thoroughly.

The return type is also interesting. Like most enumerators, Left indicates early termination and Right indicates that it ran to completion. However, the Right also contains a value of type a. The reason for this is that- by excluding the a value for early termination- we can skip computing the remaining values once output has been terminated.

## Supporting functions

The Haddock documentation on the supporting functions in Text.Hamlet.Monad is hopefully comprehensive enough to be understandable. Keep in mind that they were geared towards the support of Hamlet syntax, thus the slightly odd type signature for condH.
