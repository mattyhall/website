title: Why Zig clicks
published_date: 2021-03-21 12:00:00 +0000
description: I explain why Zig seems to click for me and makes me excited to do some personal projects again
layout: default.liquid
---

I've spent a lot of time, probably too much, learning new programming languages. Usually this isn't really a bad thing - a new language can make you think about programming in a different way or teach you new techniques. The flip side of this is learning something new takes time and slows you down, so the things you actually want to make take a backseat. However, I've recently picked up a language called [Zig](https://ziglang.org/) that has made me motivated to start and continue personal projects again.

The uncharitable amongst you might say something along the lines of "ok, but just after learning any language - whether it's Haskell, Rust, C#, or APL - aren't you enthused to make something with it?". And that's a fair point. I would be surprised, but certainly not astonished, if in two months I've abandoned by side projects and am back to playing Football Manager every evening instead. Hey, there's still a pandemic on and we all need a break! I do feel like Zig has some properties that makes it less likely than the others and the best way to sum them up is they just mean the language "clicks" for me. 

Clickiness is clearly a fuzzy and highly subjective concept, so I will try and give examples. Some of the points are light on code and heavy on feeling though, so if you are more interested in code feel free to skip forward or maybe instead read [this excellent post](https://scattered-thoughts.net/writing/assorted-thoughts-on-zig-and-rust/) comparing Rust and Zig which shows off some cool things.

Note that what clicks for me may put you off, and any comparisons I make to other languages are not to do them down at all, but as a counterpoint to Zig. 

## A small core
Zig's documentation is a bit unpolished at the moment. Understandably so given it's a pre-1.0 language and is in flux. However, that shouldn't necessarily put you off. I found that the language itself is actually pretty easy to learn for someone who has programmed before and a lot of that is to do with the language, by modern standards, being very small.

In this sense Zig reminds me a little bit of Go. I'm not really a fan of Go but one thing the language and the community does very, very well is onboarding. Part of that is the language itself was designed [to reduce complexity](https://golang.org/doc/faq#principles). Another part of that is the documentation. I remember being very impressed going to [golang.org](https://golang.org) and being taught the language in my browser and then feeling like I could go away and be productive almost immediately. Zig feels like it could be similar.

The obvious question is what do we give up in order to make the language simple? The price we pay may not suprise you given Zig's positioning as a C replacement - there are no classes, inheritance or traits for example. However Zig has many of the things that I personally find have improved my code the most - either making it far clearer, safer or making writing it quicker:

* tagged unions
* optionals
* automatic deserialisation to structs
* type checked string formatting

Here's an example with all four:

```rust
const std = @import("std");

const Framework = enum { Wpf, Django, };
const CV = union(enum) {
  software_engineer: struct { 
      five_years_of_zig_experience: bool,
      // ?T is the syntax for an optional
      favourite_framework: ?Framework,
  },
  chef: struct { has_michelin_star: bool },
};
const Person = struct {
  name: []const u8,
  cv: CV,
};

pub fn main() !void {
  // "\\" is for multiline strings
  var stream = std.json.TokenStream.init(
    \\{ 
    \\  "name": "Raymond Day", 
    \\  "cv": {
    \\    "five_years_of_zig_experience": false,
    \\    "favourite_framework": null
    \\  }
    \\}
  );
  // We need an allocator to parse the json into. Here we'll just use one backed by
  // a static buffer
  var buf: [1024]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buf);
  const x = try std.json.parse(Person, &stream, .{.allocator = &fba.allocator});
  // If we missed a case the Zig compiler would error
  switch (x.cv) {
    .software_engineer => |se| {
      std.log.debug(
        "5+ years of zig: {}, favourite framework: {}", 
        .{se.five_years_of_zig_experience, se.favourite_framework});
    },
    .chef => |c| std.log.debug("michelin star: {}", .{c.has_michelin_star}),
  }
}
// outputs "5+ years of zig: true, favourite framework: null"
```

Programming for me is a lot more enjoyable when you can get something going quickly. Small languages that are still expressive really help with that and Zig definitely ticks those two boxes.

## Local reasoning
Related to the above is Zig feels easy to reason about. I've spent quite a lot of time writing C# and, to a lesser extent, C++. Quite often going back to C is a bit of a relief. I used to wonder why this was and I think part of it is that, generally, C is a lot easier to read and reason about on a local level. I've always felt that I could look at a function in C and have an idea of what it does by reading it from top to bottom. The intention, although sometimes not expressed succinctly, is easy to work out. There are obvious caveats to this - undefined behaviour, memory safety etc can throw things completely off track, and it's certainly possible to get tricksy with pointer arithmetic - but they are ones that Zig is looking at addressing.

I hesitate to write this paragraph given code, like types, is *not* documentation but I think the Zig standard library is a good example of this. Due to the lack of Zig stdlib documentation I sometimes struggle to work out what function I need to use or how to use it, and my solution has been to read the standard library code itself to work it out. Now - once more for those at the back - code is *definitely not* documentation but I was pleasantly surprised at how easy it was to read. Even when more complicated techniques are used the intention was always clear, even if I was unfamilar with the exact semantics.

## Generics
When using other languages I've often felt like generics were tacked on after the fact. In many cases it turns out they were. Even C#, which you may think of as a language that was meant to improve Java, didn't ship with generics. Perhaps it's something else but it's always made me feel like they weren't quite in sync with the rest of the language; a bit of a kludge. Patterns that were elegant and idiomatic in a language became a bit uglier with generics.

Zig's generics feel different to me. Perhaps it's that comptime (Zig's compile time feature) is very pervasive in the language, to the point that you don't necessarily need to mark functions as available at compile time. Or perhaps it's that they're just something we use all the time - functions. Either way, I find it really compelling. Here's a quick example:

```rust
const std = @import("std");

fn LimitedSizeBuffer(comptime T: type, comptime N: usize) type {
  return struct {
    buf: [N]T,
    used: usize,

    const Self = @This();

    fn init() Self {
      return .{ .used = 0, .buf = undefined };
    }

    fn addOne(self: *Self, v: T) !void {
      if (self.used >= N) return error.NoSpace;
      self.buf[self.used] = v;
      self.used += 1;
    }
  };
}

pub fn main() !void {
  var buf = LimitedSizeBuffer([]const u8, 2).init();
  _ = try buf.addOne("benny");
  _ = try buf.addOne("harvey");
  _ = try buf.addOne("rip"); // error
}
```

Here we define a generic fixed-size buffer called LimitedSizeBuffer. As you can see really this is a function which takes the buffer type and size, which must be known at compile time, and returns a struct. It feels very elegant and very, well, Zigy. Perhaps having a small language makes it a lot easier to make it feel coherent - although I think that does the creator of Zig a disservice.

## C interop
I've spent some time looking into what it would take to integrate Rust into a C/C++ codebase and the results were not terribly encouraging. Rust, for good reason, requires your C API to be marked as `unsafe`, with the idea being you would write a safe API on top of it for consumers. Clearly this is the right choice in Rust but it can be laborious. I think this isn't helped by the fact that although you can generate bindings automatically using [bindgen](https://github.com/rust-lang/rust-bindgen), it's not clear to me whether that should always be used. Is bindgen meant to be something that gets you on your way which you then tidy up or do you run it whenever you change the C code, or even as a build step of your Rust code? The bindings generated can be very verbose and it is always in one file. If I had an image library with say a different directory for each file format and some shared data structures in another directory then it would be nice to be able to generate separate files for each. I could then use the data structures as a separate module in my own Rust code without needing to also depend on the file formats.

The obvious retort is that the code should be modular, with a small API for each part that is used to communicate with it. This in general might be a good principle but isn't always the case in the real world, and sometimes for a good reason! Even in the ideal world there is another problem though, and that is Cargo. Although it has been hugely successive in spawning a rich ecosystem of crates, focussing on Cargo as the almost only way to build Rust projects means it's pretty difficult to integrate Rust into an existing build system. It's not only me that has found this, it was mentioned by [large companies](https://gist.github.com/rylev/0e3c3895dcb40b6a1c1cf8c427c01b5e) in 2019 and I'm not sure how far thing have improved. In the linked document Google say they let Cargo build some stuff in Fuchsia but it appears that is no longer the case - [they have a tool](https://fuchsia.googlesource.com/fuchsia/+/master/tools/cargo-gnaw/README.md) which generates GN files from vendored Cargo-built crates.

Zig is a lot happier binding to C. To give you a taste I googled for a small, high quality C library and found [this ini parser](https://github.com/benhoyt/inih).  They give the following example in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../ini.h"

typedef struct
{
  int version;
  const char* name;
  const char* email;
} configuration;

static int handler(void* user, const char* section, const char* name,
                   const char* value)
{
  configuration* pconfig = (configuration*)user;

  #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
  if (MATCH("protocol", "version")) {
      pconfig->version = atoi(value);
  } else if (MATCH("user", "name")) {
      pconfig->name = strdup(value);
  } else if (MATCH("user", "email")) {
      pconfig->email = strdup(value);
  } else {
      return 0;  /* unknown section/name, error */
  }
  return 1;
}

int main(int argc, char* argv[])
{
  configuration config;

  if (ini_parse("test.ini", handler, &config) < 0) {
      printf("Can't load 'test.ini'\n");
      return 1;
  }
  printf("Config loaded from 'test.ini': version=%d, name=%s, email=%s\n",
      config.version, config.name, config.email);
  return 0;
}
```

and translated to Zig:

```rust
const std = @import("std");
const c = @cImport(@cInclude("ini.h"));

const Configuration = struct {
  version: isize,
  name: ?[]const u8,
  email: ?[]const u8,
};

fn strdup(s: []const u8) []const u8 {
  const new = std.heap.c_allocator.alloc(u8, s.len) catch unreachable;
  std.mem.copy(u8, new, s);
  return new;
}

fn handler(user: ?*c_void, section: [*c]const u8, name: [*c]const u8, 
           value: [*c]const u8) callconv(.C) c_int {
  var conf = @ptrCast(*Configuration, @alignCast(@alignOf(*Configuration), user));
  var s = std.mem.span(section);
  var n = std.mem.span(name);
  var v = std.mem.span(value);
  if (std.mem.eql(u8, "protocol", s)) {
      conf.version = std.fmt.parseInt(isize, v, 10) catch unreachable;
  } else if (std.mem.eql(u8, "user", s)) {
      if (std.mem.eql(u8, "name", n)) {
          conf.name = strdup(v);
      } else if (std.mem.eql(u8, "email", n)) {
          conf.email = strdup(v);
      }
  }
  return 0;
}

pub fn main() anyerror!void {
  var config: Configuration = undefined;
  _ = c.ini_parse("examples/test.ini", handler, &config);
  std.log.debug("ver={}, name={s}, email={s}", .{ 
    config.version, config.name, config.email 
  });
}
```

Importing a c library is as easy as `const c = @cImport(@cInclude("HEADER_NAME.h"));` and you then have full access to the functions inside it - Zig parses the header and understands C's types. There's no need here to redeclare each function as you would in other languages' FFIs. As you can see, we have to mark C pointers explicity using `[*c]`. We can then use `std.mem.span` to get a normal slice from it. I think this strikes a nice balance between being clear something is a C pointer - with all the pitfalls that entails - and allowing you to assert that you know it's safe to just get a normal Zig type from it.

Building it is easy. inih is setup to create a shared library using meson:

```
$ meson build
$ ninja -C build
$ zig build-exe src/main.zig -I. -lc -Lbuild -linih
$ ./main
debug: ver=6, name=Bob Smith, email=bob@smith.com
```

But we can do one better and use Zig's own build system here. A download of Zig includes a C compiler, which means we can mix C and Zig freely in our projects! Check it out:

```rust
const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("inih", "src/main.zig");
    exe.addIncludeDir(".");
    exe.addCSourceFile("ini.c", &.{});
    exe.linkLibC();
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
```

That's it! We use `addCSourceFile`, add an include directory and link libc and that's all. It's really refreshing to see a language that seems to want to support you in using it in big, possibly monolithic, C codebases right from the get go. Whether you want to integrate it into your current build system just like any other compiler or you're ready to fully commit and start using Zig as your build system too.

## A note of caution
Of course Zig is still quite early on in its development and we need to be patient to see how things pan out. A few things which might throw up problems occur to me:

#### Does simplicity scale?
As I've mentioned, in my own project I find it quite rewarding when I can see how some code works easily from a read through, even if that code is a little verbose. I do wonder how that scales over bigger projects and a longer time period - will the simplicy make the overall program more difficult to understand? 

The early signs are good for me I think. There's also the Zig stdlib and its compiler (which is currently being rewritten in Zig), although both were written by the language's creator so may be considered cheating.

#### RAII is good, actually
Zig has the `defer` keyword that allows you to run any code at the end of a scope. This is commonly used to free objects, eg:

```rust
var slice = allocator.alloc(u8, 14);
defer allocator.free(slice);
std.mem.copy(u8, slice, "adventure call");
```

I think this is mostly okay. Remembering to use defer became muscle memory after a bit. But I do think I'd miss RAII when it comes to reference counted pointers. Having to manually call `rc.inc()` and then matching with a `defer rc.dec()` seems quite error-prone.

## Overall
Overall I've really been enjoying Zig and think it's got a bright future. People seem to be doing interesting stuff with it, including [a Wayland tiling window manager](https://github.com/ifreund/river), [cross compiling go projects](https://dev.to/kristoff/zig-makes-go-cross-compilation-just-work-29ho) and even [using it in production](https://www.youtube.com/watch?v=124wdTckHNY).
