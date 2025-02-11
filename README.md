# learning-rust-browser

Read the book『［作って学ぶ］ブラウザのしくみ──HTTP、HTML、CSS、JavaScriptの裏側』 and try to implement a browser in Rust.

Ref: <https://github.com/d0iasm/sababook>

## Chapter 2: URL Parsing

```text
http://<host>:<port>/<path>?<searchpart>
```

ref:
<https://datatracker.ietf.org/doc/html/rfc1738>

<https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml>

## Chapter 3: Implement HTTP

HTTP/1.1:

<https://datatracker.ietf.org/doc/html/rfc7230>

<https://datatracker.ietf.org/doc/html/rfc7235>

<https://datatracker.ietf.org/doc/html/rfc9110>

<https://www.w3.org/Protocols/HTTP/AsImplemented.html>

<https://blog.cloudflare.com/ja-jp/http3-usage-one-year-on/>

<https://datatracker.ietf.org/doc/html/rfc2068> Introduced keep-alive

HTTP/2:

<https://datatracker.ietf.org/doc/html/rfc7540>

<https://datatracker.ietf.org/doc/html/rfc9113>

HTTP/3:

<https://datatracker.ietf.org/doc/html/rfc9000>

<https://datatracker.ietf.org/doc/html/rfc9114>

Request Line/Request Method/Status Code:

<https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1>

<https://tools.ietf.org/html/rfc7231#section-4>

DNS:

<https://datatracker.ietf.org/doc/html/rfc1035>

<https://datatracker.ietf.org/doc/html/rfc1034>

TCP:

<https://datatracker.ietf.org/doc/html/rfc793>

## Chapter 4: Analyze HTTP

HTML Living Standard:

<https://html.spec.whatwg.org>

DOM Living Standard:

<https://dom.spec.whatwg.org>

13.2.5 Tokenization:

<https://html.spec.whatwg.org/multipage/parsing.html#tokenization>

Node:

<https://dom.spec.whatwg.org/#interface-document>

<https://dom.spec.whatwg.org/#interface-element>

<https://dom.spec.whatwg.org/#interface-text>

<https:://dom.spec.whatwg.org/multipage/nav-history-apis.html#window>

HTML:

<https://dom.spec.whatwg.org/multipage/semantics.html#the-html-element>

Head:

<https://dom.spec.whatwg.org/multipage/semantics.html#the-head-element>

Style:

<https://dom.spec.whatwg.org/multipage/semantics.html#the-style-element>

Script:

<https://dom.spec.whatwg.org/multipage/semantics.html#the-script-element>

Body:

<https://dom.spec.whatwg.org/multipage/semantics.html#the-body-element>

HTML Parsing:

<https://html.spec.whatwg.org/multipage/parsing.html#original-insertion-mode>

<https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements>

13.2.6 Tree Construction:

<https://html.spec.whatwg.org/multipage/parsing.html#tree-construction>

13.2.4.1 The insertion mode:

<https://html.spec.whatwg.org/multipage/parsing.html#the-insertion-mode>

Document state:

<https://html.spec.whatwg.org/#docment>

13.2.6.4.2 The "before html" insertion mode:

<https://html.spec.whatwg.org/multipage/parsing.html#the-before-html-insertion-mode>

Insert a foreign element:

<https://html.spec.whatwg.org/multipage/parsing.html#insert-a-foreign-element>

Insert a character:

<https://html.spec.whatwg.org/multipage/parsing.html#insert-a-character>

### ./run_on_wasabi.sh

```bash
[INFO]  os/src/usb_hid_keyboard.rs:163:  usb_hid_keyboard is ready
saba
[INFO]  os/src/cmd.rs:58 :  Executing cmd: ["saba"]

Document
  Element(Element { kind: Html, attributes: [] })
    Element(Element { kind: Head, attributes: [] })
    Element(Element { kind: Body, attributes: [] })
      Element(Element { kind: H1, attributes: [Attribute { name: "id", value: "title" }] })
        Text("H")
      Element(Element { kind: H2, attributes: [Attribute { name: "class", value: "class" }] })
        Text("H")
      Element(Element { kind: P, attributes: [] })
        Text("T")
      Element(Element { kind: P, attributes: [] })
        Element(Element { kind: A, attributes: [Attribute { name: "href", value: "example.com" }] })
          Text("L")
        Element(Element { kind: A, attributes: [Attribute { name: "href", value: "example.com" }] })
          Text("L")
[INFO]  os/src/cmd.rs:154:  Ok(0)
```

## Chapter 5: Decorate with Css

CSS Specification:

<https://www.w3.org/Style/CSS/specs.en.html>

CSS Syntax Module Level 3:
(Refer to implement tokenizer)

<https://www.w3.org/TR/css-syntax-3/>

4.Tokenization

<https://www.w3.org/TR/css-syntax-3/#tokenization>

CSS Object Model (CSSOM):
(Refer to implement the CSSOM)

<https://www.w3.org/TR/cssom-1/>

Selectors Level 4:
(Refer to implement the selector)

<https://www.w3.org/TR/selectors-4/>

2.Selectors Overview:

<https://www.w3.org/TR/selectors-4#overview>

5.1. Type (tag name) selector:

```css
p {
    color: red;
}
```

<https://www.w3.org/TR/selectors-4/#type-selectors>

6.6. Class selectors:

```css
.example {
    color: red;
}
```

<https://www.w3.org/TR/selectors-4/#class-html>

6.7. ID selectors:

```css
#example {
    color: red;
}
```

<https://www.w3.org/TR/selectors-4/#id-selectors>

15.1. Descendant combinator:

```css
div p {
    color: red;
}
```

<https://www.w3.org/TR/selectors-4/#descendant-combinators>

CSS Cascading and Inheritance Level 4:
(Refer to implement the cascade and inheritance)

<https://www.w3.org/TR/css-cascade-4/>

W3C List of CSS properties, both proposed and standard:

<https://www.w3.org/Style/CSS/all-properties.en.html>

CSS Values and Units Module Level 4:

<https://drafts.csswg.org/css-values/>

CSS Color Module Level 4:

<https://drafts.csswg.org/css-color/#the-color-property>

6.1.2 The CSSStyleSheet Interface

<https://www.w3.org/TR/cssom-1/#cssstylesheet>

ComputedStyle:
Cascading:

<https://www.w3.org/TR/css-cascade-4/#cascading>

Defaulting:

<https://www.w3.org/TR/css-cascade-4/#defaulting>

## Chapter 6: Create window of GUI application

## Chapter 7: Run JavaScript

Feature:

1. The capability to carry out basic arithmetic operations, including addition and subtraction.
2. The ability to define and use variables.
3. The ability to define and call functions.

AdditiveExpression: (ECMAScript Language Specification)

<https://262.ecma-international.org/#sec-additive-operators>

AST(Abstract syntax Tree) Explorer:

<https://astexplorer.net/>
