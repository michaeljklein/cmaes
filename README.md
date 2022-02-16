# cmaes

An implementation of the CMA-ES optimization algorithm. It is used to minimize the value of an objective function and performs well on high-dimension, non-linear, non-convex, ill-conditioned, and/or noisy problems. See [this paper][0] for details on the algorithm itself. This library is based on the linked paper and the [pycma][1] implementation.

# Quick Start

Add this to your Cargo.toml:

```
[dependencies]
cmaes = "0.1.0"
```

Then, to optimize a function:
```rust
use cmaes::{CMAESOptions, DVector, PlotOptions};

let sphere = |x: &DVector<f64>| x.iter().map(|xi| xi.powi(2)).sum();

let dim = 10;
let mut cmaes_state = CMAESOptions::new(dim)
    .initial_mean(vec![1.0; dim])
    .enable_printing(200)
    .enable_plot(PlotOptions::new(0, false))
    .build(sphere)
    .unwrap();

let max_generations = 20000;
let result = cmaes_state.run(max_generations);

cmaes_state.get_plot().unwrap().save_to_file("plot.png", true).unwrap();
```

The produced plot will look like this:

[sphere function plot][2]

For more information, see the [documentation][3] and [examples][4].

# Contributing

Contributions are welcome! You can contribute by reporting any bugs or issues you have with the library, adding documentation, or opening pull requests.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as below, without any additional terms or conditions.

# License

Licensed under either of

    Apache License, Version 2.0, ([LICENSE-APACHE][5] or http://www.apache.org/licenses/LICENSE-2.0)
    MIT license ([LICENSE-MIT][6] or http://opensource.org/licenses/MIT)

at your option.

[0]: https://arxiv.org/pdf/1604.00772.pdf
[1]: https://github.com/CMA-ES/pycma
[2]: https://github.com/pengowen123/cmaes/tree/master/images/plot_sphere.png
[3]: https://docs.rs/cmaes/latest/cmaes
[4]: https://github.com/pengowen123/cmaes/tree/master/examples
[5]: https://github.com/pengowen123/cmaes/tree/master/LICENSE-APACHE
[6]: https://github.com/pengowen123/cmaes/tree/master/LICENSE-MIT
