title: Creating models of F1 tracks in Rust - Part 1
date: 19 Aug 2014 12:00:00 +0100
tags: rust, programming, formula1
description: We start off by loading an open street map xml file into Rust
extends: default.liquid
---

**DISLAIMER: Compiled with rustc version 0.12.0-pre-nightly (commit 01ec6fab2). Whether it works with later versions is anyone's guess. I'm only a beginner to Rust so do not think that I know what I'm doing or that my code is idomatic. :)**

I'm a massive Formula 1 fan so it won't surprise those who know about the sport that the past few Sundays have been rather boring. You see it is the mid season break which means three consecutive weekends with no racing. Luckily I found a way to fight the withdrawal symptoms - some F1 themed programming! But what?

Well you can probably guess from the title - 3D models of the tracks - but there is a good reason for it. It is very rare that you get to see all of the track on telly, and if you do then it is usually only a 2D drawing. I thought it would be cool to see how much the track goes up and down. Quite often you will hear a commentator say "The camera image does not do the climb justice" or something to that efffect. Let's see if they're right!

## Why Rust?
I know how to program in Python - a high level, imperative language - and Haskell - a high level, functional language. I thought it was about time I learnt a systems programming language to complete the set of programming language types I've learnt (I'm not going to say "know" as that is probably too grand a term). As I said in the disclaimer this code may not be idiomatic. I've tried to be as accurate as possible and I'm happy to be corrected/berated.

For those who don't know [Rust](http://www.rust-lang.org/) is a language with algebraic data types, pattern matching, support for functional programming (for some definition of functional) and safety. It is meant to be used in places where C++ may have been used but unlike C++ you can't accidently access memory that has been deallocated easily. Be wary though: Rust hasn't stabalised so it isn't ready for production.

It could probably be said that this is not a task that Rust will be particularly suited for and it'd be much quicker to do this sort of thing in a scripting language. I decided to ignore this train of thought however because I was worried it was my brain trying to save itself the work of learning a new language. It was actually quite enjoyable in the end.

## Parsing OSM files
So the first thing we need to do is get some data for the track. Luckily [openstreetmaps.org](http://openstreetmaps.org/) allow you to download their data in a nice XML format. It's as easy as searching for the track, eg. "Spa", drawing a box around the circuit and then clicking "export". Here's an example of what the file looks like:

```xml
<osm version="0.6" generator="CGImap 0.3.3 (32175 thorn-03.openstreetmap.org)" copyright="OpenStreetMap and contributors" attribution="http://www.openstreetmap.org/copyright" license="http://opendatacommons.org/licenses/odbl/1-0/">
 <node id="147753704" visible="true" version="1" changeset="47586" user="gaku" lat="50.4226185" lon="5.9660655">
  <tag k="created_by" v="JOSM"/>
 </node>
 <way id="174892450" visible="true" version="1" changeset="12643060" user="Scrup">
  <nd ref="1855362418"/>
  <nd ref="1855362431"/>
 </way>
 <relation id="284560" visible="true" version="11" changeset="12942711" user="Scrup">
  <member type="node" ref="258602622" role="start"/>
  <member type="way" ref="126807101" role=""/>
  <member type="way" ref="126807110" role=""/>
  <member type="way" ref="34403885" role="pit_lane"/>
  <tag k="name" v="Ciruit de Spa Francorchamps"/>
  <tag k="type" v="circuit"/>
 </relation>
```

Relations group together different entities together. Ways are list of nodes. Nodes are a point on Earth. Each may have a number of tags associated with it to store extra information. Our first step should be to write some code which parses this information into a data structure. I didn't really fancy writing my own XML parser so I looked about for a Rust library (which are called crates) to do so. It turns out there are quite a few including, but not limited to, [rust-xml](https://github.com/netvl/rust-xml), [RustXML](https://github.com/Florob/RustyXML) and [sax-rs](https://github.com/bjz/sax-rs).

Before writing this program I had never heard of SAX. I thought all XML parsers turned XML into a DOM (a list of tags which each have their own children and so on). Instead SAX will read "events" from the XML passed in, where an event is something like "Start element with these attributes" or "End element". I ended up opting for sax-rs but I don't know if this was the correct choice. When I wrote this it had been updated fairly recently and it compiled but whether it is the best/fasted library for the job I do not know.

**EDIT: The author of sax-rs has recommended using [netvl/rust-xml](https://github.com/netvl/rust-xml/)**

Let's get the imports out of the way first:

```rust
#![feature(struct_variant)]
extern crate sax;

use std::collections::HashMap;
use std::io::IoError;
```

Let's create an enum to hold each type of element:

```rust
pub type Tags = HashMap<String, String>;

pub enum OsmElement {
    Node { pub id: int, pub lat: f64, pub lng: f64, pub visible: bool, pub tags: Tags },
    Way { pub id: int, pub nodes: Vec<int>, pub tags: Tags },
    Relation { pub id: int, pub members: Vec<int>, pub tags: Tags },
}
```

This is the first example of Algebraic Data Types in Rust. Normally enums in Rust look like this:

```rust
pub enum OsmElement {
    Node(int, f64, f64, f64, bool, Tags),
}
```

But, ``struct_variant`` allows you to give names to the values the enum holds. In this case I wonder if having separate structs (eg Node, Way, Relation) and then having the enum variants store one of those structs would be better. 

I decided to store the tags as a hash map of strings. In Rust there are two different string types: ``String`` and ``&str``. The former is mutable but allocates memory and the latter is immutable and references another string. As far as I know ``&str`` is prefered when possible however sax-rs stores attributes as ``String``s so I shall follow their lead. It also makes writing the code a bit easier (if I used ``&str`` I would have to annotate how long it should live).

```rust
pub struct Osm {
    parser: Receiver<sax::ParseResult>,
    pub elements: HashMap<int, OsmElement>
}

impl Osm {
    pub fn new(path: &Path) -> Osm {
        let parser = sax::parse_file(path).unwrap();
        let mut s = Osm {parser: parser, elements: HashMap::new()};
        s.parse();
        s
    }
    
    fn parse(&mut self) {
        match self.parser.recv() {
            Ok(sax::StartDocument) => (),
            _ => fail!("Document did not start")
        }
        ...
    }
```

We create a struct to store our elements in. The weird ``Receiver`` type you see is one of Rust's channel types. Basically parser is something which we can pull ``ParseResult``s out of. The sax-rs library returns one of these when we call ``parse_file``, wrapped inside an ``Option`` which is like ``Maybe`` from Haskell. An ``Option`` can either have a value ``Some(T)`` or be empty ``None`` which allows us to give some notion of whether something was successful or not. In this case we call ``unwrap`` which will give back the value stored or will stop the program running. This is something that should be avoided unless you know for definite that the ``Option`` is a ``Some``.

We then create an ``Osm`` and call ``parse``. The ``&mut self`` parameter tells us that the ``parse`` should be called on an ``Osm`` which is mutable. The function gets passed a reference to this ``Osm``. In Rust variables are immutable by default. The first thing ``parse`` does is ask the ``Receiver`` for a value. We then use pattern matching to see what we got. 

We pattern match on a ``sax::ParseResult`` which is a special version of the ``Result`` type included in the standard library. A result can be either ``Ok`` or an ``Err``. If we get a StartDocument value we return ``()`` - an empty tuple - and if we get anything else we fail the program with a message. Wouldn't it be nice though to not just fail though? It'd be better if we reported the error:

```rust
#[deriving(Show)]
pub enum OsmParseError {
    IoErr(IoError),
    SaxErr(sax::error::ErrorData),
    ParseErr(String),
}

// not to be confused with sax::ParseResult
pub type ParseResult = Result<(), OsmParseError>;

impl Osm {
    pub fn new(path: &Path) -> Result<Osm, OsmParseError> {
        let parser = sax::parse_file(path).unwrap();
        let mut s = Osm {parser: parser, elements: HashMap::new()};
        try!(s.parse());
        Ok(s)
    }
    
    fn parse(&mut self) -> ParseResult {
        match self.parser.recv() {
            Ok(sax::StartDocument) => (),
            Ok(e) => return Err(ParseErr(format!("Document started with: {}", e))),
            Err(e) => return Err(SaxErr(e))
        }

        for event in self.parser.iter() {
            match event {
                Ok(sax::StartElement(name, attrs)) => try!(self.parse_start_element(name, attrs)),
                Ok(_) => (),
                Err(e) => return Err(SaxErr(e)),
            }
        }
        Ok(())
    }
```

First we create an enum to store each type of error we could get. In ``new`` we use ``try!`` which is a macro. In fact everything with an exclamation mark on the end is a macro (for example ``fail!``). ``try!`` will look at the ``Result`` it is passed and if it is an ``Err`` it will return the current function (in this case ``new``) with that ``Err``. If it is ``Ok`` it'll just give back the value. We end the ``new`` function by wrapping it in an ``Ok`` so it is a ``Result``.

This time ``parse`` will return an error if the document did not start. The ``iter`` function on a ``Receiver`` returns an iterator which will get a result each time it is asked to. In Rust an iterator is something that has a ``next`` method which returns a value in an ``Option`` when it is called. If the ``Option`` is ``None`` and it is being used in a for loop then the for loop stops.

If the event is the start of an element we call ``parse_start_element``, again with the ``try!`` macro. Any other event we can ignore as it should be a comment or some text. We return any errors.

```rust
    fn parse_start_element(&mut self, name: String, attrs: sax::Attributes) -> ParseResult {
        match name.as_slice() {
            "relation" => try!(self.parse_relation(attrs)),
            "node" => try!(self.parse_node(attrs)),
            "way" => try!(self.parse_way(attrs)),
            _  => ()
        }
        Ok(())
    }
```

``parse_start_element`` just calls other methods depending on which element was found. Note that ``as_slice`` is called on ``name`` because string literals are of type ``&str`` and ``name`` is a ``String``. Let's have a look at ``parse_node``:


```rust
    // parses <node id=1 lat=50.23232 lon=0.12121 visible=true><tag k="key" v="val"/></node>
    fn parse_node(&mut self, attrs: sax::Attributes) -> ParseResult { 
        let id = attrs.find("id").and_then(|v| from_str(v));
        let lat = attrs.find("lat").and_then(|v| from_str(v));
        let lng = attrs.find("lon").and_then(|v| from_str(v));
        let visible = attrs.find("visible").and_then(|v| from_str(v));
        let (id, lat, lng, visible) = match (id, lat, lng, visible) {
            (Some(id), Some(lat), Some(lng), Some(visible)) => (id, lat, lng, visible),
            _ => return Err(ParseErr(
                "Could not find all required attributes on node".to_string()))
        };
        let mut tags = HashMap::new();

        for event in self.parser.iter() {
            match event {
                Ok(sax::StartElement(name, attrs)) => {
                    match name.as_slice() {
                        "tag" => try!(self.parse_tag(attrs, &mut tags)),
                        _ => return Err(ParseErr(format!(
                              "Expecting all children of nodes to be tags. Got a {}",
                              name))),
                    }
                }
                Ok(sax::EndElement(name)) => {
                    if name.as_slice() == "node" {
                        break;
                    }
                    return Err(ParseErr(format!(
                        "Expecting node to end, not a {}", name)));
                }
                _ => {},
            }
        }

        self.elements.insert(id, Node{id: id, lat: lat, lng: lng,
                                      visible: visible, tags: tags});

        Ok(())
    }
```

First off we grab all the attributes we want using ``find``. As this may fail we get an ``Option`` back. Next we use ``and_then`` which is a method that takes a closure (an anonymous function) and calls it with the value if there was one. We use this to convert a string into a Rust data type.

We then iterate through events again. If a tag starts then we parse that and if the node tag ends we stop iterating and insert the node into the elements hash map with it's id as the key so we can look up specific elements later. Anything else is an error because only tags can be children and the ``parse_tag`` function handles ending the tag element.

That was quite a lot of code and I don't really fancy writing it again. Unfortunately both ``parse_way`` and ``parse_relation`` are likely to be quite similar as they have to check they get the right children and the element ends. My first thought was to have a function that took a closure which would handle checking the children. 

It turns out this is somewhat trickier than I expected due to the fact that Rust only allows one mutable reference at a time, so this function could not be a method on ``Osm``. At the time of writing the code I could not work this out and was baffled, it was only during writing this that I realised how to do it. You can take a look at it [here](https://gist.github.com/mattyhall/46fafc61c6e0eb1db35c).

I ended up asking for a code review on the Rust subreddit. The very helpful Chris Morgan suggested that I used a macro which I adapted slightly to be more general:


```rust
macro_rules! parse {
    ($iter:expr, $close_tag:expr $(, $tag:pat => $method:expr)*) => {
        for event in $iter {
            match event {
                Ok(sax::StartElement(name, attrs)) => {
                    match name.as_slice() {
                        $($tag => try!($method(attrs)),)*
                        _ => return Err(ParseErr(format!(
                              "Unexpected child in {} Got a {}",
                              $close_tag, name))),
                    }
                }
                Ok(sax::EndElement(name)) => {
                    if name.as_slice() == $close_tag {
                        break;
                    }
                    return Err(ParseErr(format!(
                            "Expecting {} to end, not a {}", 
                            $close_tag, name)));
                }
                _ => {},
            }
        }
    }
}
```

The first line defines that this is a macro. We then specify the parameters to the macro which all start with a dollar sign. We have ``$iter`` which is an ``expr`` or expression. Then is the tag name which we want to close. The last part is a bit more confusing. The ``$(..)*`` means match ``..`` zero or more times. In this case ``..`` is something like ``"tag" => |attrs| self.parse_tag(attrs)``. Note that ``$tag`` is a ``pat`` - something we can pattern match on. The body of the macro should look familiar. The only strange bit is ``$($tag => try!($method(attrs)),)*``. For every value of ``$tag`` and ``$method`` this will expand to ``$tag => try!($method(attrs)),``.

The macro is used like so:

```rust
    fn parse_relation(&mut self, attrs: sax::Attributes) -> ParseResult {
        let id = try!(self.parse_int_attr("id", attrs));
        let mut tags = HashMap::new();
        let mut members = Vec::new();

        parse!(self.parser.iter(), "relation",
               "member" => |attrs| Ok(members.push(try!(self.parse_member(attrs)))),
               "tag" => |attrs| self.parse_tag(attrs, &mut tags));

        self.elements.insert(id, Relation{id: id, members: members, tags: tags});
        Ok(())
    }

    fn parse_tag(&mut self, attrs: sax::Attributes, tags: &mut Tags) -> ParseResult {
        let (k, v) = match (attrs.find_clone("k"), attrs.find_clone("v")) {
            (Some(k), Some(v)) => (k, v),
            _ => return Err(ParseErr("Tag must have a k and a v attribute".to_string()))
        };

        parse!(self.parser.iter(), "tag");

        tags.insert(k, v);
        Ok(())
    }
```

The rest of the parsing code is quite repetitive so I'll let you [browse it](https://github.com/mattyhall/osmxml) at your own leisure. Feel free to contact me if there is something you don't understand.

## Getting the points of the track
I took a lot more space talking about the parser than I expected so I'll have to leave converting this data into models (the fun bit!) to another blog post. I feel like leaving all of it to the next post is a bit of a cop out so I'll quickly show you how to get a list of points from an OSM file.

Well, I say a list of points from an OSM file. I actually mean a list of lists of points because two consecutive elements in a way element should be joined up. However this is not true of two consecutive elements in a relation as far as I can tell. This isn't really important right now but will be in later blog posts.

First you will need to create a project using Cargo, the Rust package manager. If you installed Rust from the website you should also have Cargo installed. Run:

```
$ cargo new osmmodels
$ cd osmmodels
```

Then add a dependency to ``Cargo.toml``. This will compile your code with my osm parser:

```
[dependencies.osmxml]
git = "https://github.com/mattyhall/osmxml"
```

The following code should go in src/main.rs

```rust
extern crate osmxml;
use osmxml::{Osm, OsmElement, Relation, Way, Node};

fn main() {
    let path = &Path::new("spa.osm");
    let osm = Osm::new(path).unwrap();
    let relation = osm.elements.values().filter(|e| {
        match **e {
            Relation{tags: ref ts, ..} => {
                ts.find(&"name".to_string()) == Some("Ciruit de Spa Francorchamps")
            }
            _ => false
        }
    }).next().unwrap();
    println!("{}", expand_relation(relation, &osm.elements));
}
```

We get the values from the hash map; filter the iterator so it only contains relations with the right name and get the first. Note that we have to dereference ``e`` (the element) twice because values creates an iterator of references and filter call the closure with a reference to the element. We then pass it to ``expand_relation``:

```rust
fn expand_relation(elem: &OsmElement, elements: &HashMap<int, OsmElement>) -> Vec<Vec<(f64, f64)>> {
    let refs = match *elem {
        Relation {members: ref m, ..} => m,
        _ => fail!("expand_relation must be passed a relation"),
    };
    let mut ways = Vec::new(); 
    for r in refs.iter() {
        match elements.find(r) {
            Some(e@&Way{..}) => ways.push(expand_way(e, elements)),
            None => fail!("Could not find element with id {}", r),
            _ => (),
        };
    }
    ways
}

fn expand_way(elem: &OsmElement, elements: &HashMap<int, OsmElement>) -> Vec<(f64, f64)> {
    let refs = match *elem {
        Way {nodes: ref ns, ..} => ns,
        _ => fail!("expand_way must be passed a way")
    };
    let mut latlngs = Vec::new();
    for r in refs.iter() {
        match elements.find(r) {
            Some(e@&Node{..}) => latlngs.push(expand_node(e)),
            None => fail!("Could not find element with id {}", r),
            _ => ()
        }
    }
    latlngs
}

fn expand_node(elem: &OsmElement) -> (f64, f64) {
    match *elem {
        Node {lat: lat, lng: lng, ..} => (lat, lng),
        _ => fail!("expand_node must be passed a node")
    }
}
```

``expand_relation`` gets each member of the relation and looks it up in the hash map. If it is a way we call ``expand_way`` on it and add the resulting vector of tuples to the ways vector, otherwise we skip. ``expand_way`` looks up each of its nodes and inserts the latitude longitude pair into the ``latlngs`` vector.

There are a few things that are worth mentioning here. As ``Some(e@&Node{..})`` shows we can match against references with ``&`` and if you have ``name@pat`` then ``name`` will be bound to the match of ``pat``. Finally notice how we have to check that the element we've been passed is the right one in ``expand_node`` even though we know it is because we checked in ``expand_way``. This is why I think that ``OsmElement`` should store named structs so you could just pass the struct.

If you run this with [spa.osm](https://github.com/mattyhall/osmxml/blob/master/spa.osm) in the current directory:

```
$ cargo build
$ ./target/osmmodels
```

You should get latitudes and longitudes flooding your screen. Yay!

Join me next time when we actually turn all this into models. If you have comments please feel free to [email me](mailto:matthew@quickbeam.me.uk).
