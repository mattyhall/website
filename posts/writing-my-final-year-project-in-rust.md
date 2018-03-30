title: Writing My Final Year Project in Rust
layout: default.liquid
published_date: 2018-03-30 23:00:00 +0000
---
As part of my final year in university I have had to undertake a project and then write a twenty page paper on it. It's what, or is instead of, what other degrees call dissertations. I think it's not called a diss because it's more project based but I'm not sure. Anyway,, we were allowed to send a list of five projects we were interested in and I ended up being assigned one on a type of machine learning algorithm called boosting. This wasn't my first choice unfortunately, so I decided I'd try to make it interesting for myself by implementing it in Rust. Rust was, and still is, quite immature when it comes to machine learning - as [Are We Learning Yet?](http://www.arewelearningyet.com/) confirms. I thought it would be an interesting challenge to write some machine learning algorithms in a language that has yet to be used too much for this field.

I haven't released the code itself, so this post will be very code light. I'm not sure if I will release it as it's a uni project and probably isn't much use to others. I have experimented with something more general with ndarray and in the future I might expand this. Unfortunately work for my final (thank god) exams have taken priority. 

## Lessons Learnt
Overall, writing the project in Rust was quite mixed, with some positives and some negatives. Along the way I learnt a few lessons, some about Rust and others about generally doing a programming project.

#### Doing things from scratch is fun
There's a common saying - don't reinvent the wheel. Although there is a lot of wisdom to this, I found doing everything (well, almost everything) myself a really enjoyable experience. I made a decision early on in the project that I'd write as much as possible so all the weak learners and all the boosting algorithms were written from scratch. In Rust this was a decision that was not entirely my own, given there are not that many machine learning crates out there and none (as far as I know) that have wide usage. Learning about the algorithms myself from the original papers, other implementations and, of course, Wikipedia was really interesting and definitely helped when it came to writing the report.

#### Rust's performance is pretty good
For some reason which is now a mystery after six months have past, I decided to write my own container for datasets, even though [ndarray](https://github.com/bluss/rust-ndarray) existed when I started. I ended up using a vector of vectors of floats (`Vec<Vec<f64>>`) to store the data. This certainly isn't the most efficient way of storing the values, but even so the algorithms all ran fairly quickly (compared to scikit-learn), with no optimisation done which was quite surprising. I expect if I had used ndarray then they would have been even quicker.

#### Rust's compile times are less pretty good
I don't want to sound too negative but the compile times were quite painful. Having to wait for half a minute before you can even run your experiments, which themselves take quite a long time, can be very frustrating and it did hurt both my motivation and productivity. 

Compile times are mentioned in Rust's [2018 roadmap](https://blog.rust-lang.org/2018/03/12/roadmap.html): "Compiler performance should not be an obstacle to productivity in Rust 2018". Productive seems to be a relative measure to me rather than an absolute, but I think compiler performance has quite a long way to go before it stops being annoying. Like most things you do eventually just get used to it, but occasionally I did re-realise that a lot of my time was spent waiting for a compile.

Whilst writing this post I came across a post on reddit complaining about compile times... two years ago. Unfortunately, I'm not that hopeful about the coming two years in this regard.

#### Benchmarking and optimisation are hard
One day I decided it'd be fun to try and parallelise some of my code - there was quite a lot of code that iterated through a vector and did some number crunching. So, I wrote some microbenchmarks, changed `iter` to `par_iter` from [rayon](https://github.com/rayon-rs/rayon) and ran the benchmarks. I was getting some pretty good increases in performance so I went ahead and kept adding parallelism. With that all done I ran the whole program. On one dataset it went from taking ten seconds to over a minute. Why this happened I'm not sure. At a guess my benchmarks were faulty or the work was too small to make the overhead of parallelising it worth it. 

Unforunately, as in many projects, getting things done on time were a higher priority than investigating things that, to me, were more interesting. If I was just doing this for fun, then I may very well have investigated further. 

#### I found using the FFI quite difficult
My original plan was to have a graphical user interface that would allow users to load up their dataset, run some boosting algorithms on it and then graph the results. Originally this led me towards implementing the whole project in Python however I thought it would be pretty straight forward to create an FFI and then use my Rust code from Python. 

Unfortunately, the official documentation for doing this seems quite sparse, which I guess is understandable given if someone wants to call Rust from another language they probably know enough to work it out themselves. Luckily for me, there's a very good [blog post](https://bheisler.github.io/post/calling-rust-in-python/) by Brook Heisler that covered pretty much everything I needed to know.

It wasn't all smooth sailing however. I did run into a weird (to me) problem where my program would segfault when a certain method on a pointer to a struct was called. Strangely, other methods worked fine and the method worked fine when called in Rust, it was just calling it in Python that was a problem. After running gdb it seemed the problem was in jemalloc, Rust's default allocator. I found that switching to the system allocator fixed my problems so didn't investigate further.

Although I did find doing this part quite difficult, a lot of that may be down to me. It may very well be that the problem with my code was something trivial but I'm not really experienced enough in interfacing between Python and other languages, and in low-level stuff generally, to know what the problem was. 

#### Rust is really great for CLI apps
As the final interface I opted for a command line/configuration file setup. I was happily surprised at how easy going implementing it was, even though it was a fairly simple task. The general idea was that a configuration file would give the file of the dataset, the types of the columns and the algorithms to run. I ended up choosing TOML as the format seeing as it's widely used in Rust and was expressive enough to be able to express the things that I needed. Getting it all working was a breeze with [toml-rs](https://github.com/alexcrichton/toml-rs), which uses serde for automatic serialisation/deseralisation with structs.

Adding command line overrides for some of the options was easy with [clap](https://github.com/kbknapp/clap-rs) and the code was pretty nice to read too.

After my brief foray into and then failure using rayon I realised a much simpler idea was to just run the algorithms at the same time. To do this I used [threadpool](https://github.com/rust-threadpool/rust-threadpool). Apart from a bit of fighting the borrow checker, I was impressed with how easy it was to just drop it in. Added with a few progress bars courtesy of [inficatif](https://github.com/mitsuhiko/indicatif) and I had something that was pretty easy to use and looked nice too.

CLI apps are a focus for Rust in 2018. All I can say is that Rust already works pretty well for me!

### Working on something everyday is challenging, but you learn a lot
Okay, so maybe I only worked on it every few days. Or every week. But I think it's still true. Having the motivation to work on something often, especially when it's not necessarily a passion project, is difficult. It gets even more difficult when the only hard deadlines are well off into the future. I found that having one day set aside to working on it every week helped.

The other side of the coin is that working on something for so long has definitely helped me. Most of my projects before have been written over the course of only a few weeks so didn't require much dedication to keep persevering with. I also learnt that I tend to think programming tasks will take longer than they actually do, and writing tasks shorter than they actually do. Perhaps that's because I've spent two years living with arts students :P

## Should We Do Machine Learning in Rust?
Maybe. I find it difficult to imagine Rust beating Python for productivity though. With all the tooling around Python - scikit-learn, pandas, numpy, IPython etc - things are very quick and easy. The compile times seem like a limiting factor, at least for data exploration type work. Rust might be a good language to write algorithms themselves in, although even Python is quick when numpy is used (because it's not actually Python). I may very well be wrong though. I thought doing small CLI stuff in Python was easy but Rust was like a breath of fresh air.

I imagine a scikit-learn type crate would be a lot of fun to implement. A lot of interfaces, such as models having a `fit` and `predict` method, models being [`clone`-able](http://scikit-learn.org/stable/modules/generated/sklearn.base.clone.html) etc, would naturally turn into traits in Rust, adding the benefit of type safety. 

If anyone plans on such a crate, please feel free to reach out. I might be able to write a few boosting algorithms for you!