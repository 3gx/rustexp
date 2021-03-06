//#![cfg_attr(feature = "nightly", feature(specialization))]

fn main() {
    {
        use gc::{Finalize, Gc, Trace};
        let x = Gc::new(1_u8);
        let y = Gc::new(Box::new(Gc::new(1_u8)));

        #[derive(Trace, Finalize, Debug, Clone)]
        struct Foo {
            a: Gc<u8>,
            b: u8,
        }

        let z = Gc::new(Foo { a: x.clone(), b: 1 });
        println!("x= {:?}", x);
        println!("y= {:?}", y);
        println!("z= {:?}", z);
    }

    {
        use gc::{Finalize, Gc, GcCell, Trace};
        #[derive(Trace, Finalize, Debug)]
        struct Foo {
            cyclic: GcCell<Option<Gc<Foo>>>,
            data: u8,
        }

        let foo1 = Gc::new(Foo {
            cyclic: GcCell::new(None),
            data: 1,
        });
        let foo2 = Gc::new(Foo {
            cyclic: GcCell::new(Some(foo1.clone())),
            data: 2,
        });
        let foo3 = Gc::new(Foo {
            cyclic: GcCell::new(Some(foo2.clone())),
            data: 3,
        });
        println!("foo3= {:?}", foo3);
        println!("foo1= {:?}", foo1);
        *foo1.cyclic.borrow_mut() = Some(foo3.clone());
    }
    {
        use gc::{Gc, GcCell};
        let x = Gc::new(GcCell::new(20));
        let y = Gc::clone(&x);
        println!("x= {:?}", x);
        *x.borrow_mut() = 42;
        println!("x= {:?}", y);
    }
    println!("Hello, world!");
    {
        let mut graph = {
            use petgraph::stable_graph::StableGraph;
            StableGraph::new()
            /*
            use petgraph::graph::Graph;
            Graph::new()
            */
        };
        let origin = graph.add_node("Denver");
        let destination_1 = graph.add_node("San Diego");
        let destination_2 = graph.add_node("New York");
        let cost_1 = graph.add_edge(origin, destination_1, 250);
        let cost_2 = graph.add_edge(origin, destination_2, 1099);
        let cost_3 = graph.add_edge(origin, destination_2, 2099);
        let cost_3a = graph.add_edge(origin, destination_2, 2099);

        println!("{:?}", origin);
        println!("{:?}", destination_1);
        println!("{:?}", destination_1);
        println!("{:?}", cost_1);
        println!("{:?}", cost_2);
        println!("{:?}", cost_3);
        println!("{:?}", cost_3a);
        assert_ne!(cost_3, cost_3a);
        // graph.remove_node(origin);

        //graph.remove_edge(cost_2);
        for e in graph.edge_indices() {
            println!("edge={:?}", graph[e]);
        }

        //let n = graph.neighbors(origin);
        //        assert_eq!(graph.node_weight(origin).unwrap(), &"Denver");
        //assert_eq!(graph[origin], "San Diego");
        assert_eq!(graph[destination_1], "San Diego");
        assert_eq!(graph[destination_2], "New York");
        assert_eq!(graph.edge_weight(cost_1).unwrap(), &250);
        assert_eq!(graph.edge_weight(cost_2).unwrap(), &1099);
        assert_eq!(graph.edge_weight(cost_3).unwrap(), &2099);

        let g1 = petgraph::Graph::from(graph);
        assert_eq!(g1[destination_1], "San Diego");
        assert_eq!(g1[destination_2], "New York");
        assert_eq!(g1.edge_weight(cost_1).unwrap(), &250);
        assert_eq!(g1.edge_weight(cost_2).unwrap(), &1099);
        assert_eq!(g1.edge_weight(cost_3).unwrap(), &2099);
    }

    {
        use petgraph::algo::dijkstra;
        use petgraph::prelude::*;
        use petgraph::stable_graph::StableGraph as Graph;
        use std::collections::HashMap;

        let mut graph: Graph<(), (), Directed> = Graph::new();
        let a = graph.add_node(()); // node with no weight
        let b = graph.add_node(());
        let c = graph.add_node(());
        let d = graph.add_node(());
        let e = graph.add_node(());
        let f = graph.add_node(());
        let g = graph.add_node(());
        let h = graph.add_node(());
        // z will be in another connected component
        let _z = graph.add_node(());

        graph.extend_with_edges(&[
            (a, b),
            (b, c),
            (c, d),
            (d, a),
            (e, f),
            (b, e),
            (f, g),
            (g, h),
            (h, e),
        ]);
        // a ----> b ----> e ----> f
        // ^       |       ^       |
        // |       v       |       v
        // d <---- c       h <---- g

        let expected_res: HashMap<NodeIndex, usize> = [
            (a, 3),
            (b, 0),
            (c, 1),
            (d, 2),
            (e, 1),
            (f, 2),
            (g, 3),
            (h, 4),
        ]
        .iter()
        .cloned()
        .collect();
        let res = dijkstra(&graph, b, None, |_| 1);
        assert_eq!(res, expected_res);
    }
    {
        let mut vec = vec!["a".to_string(), "b".to_string()];
        println!("vec={:?}", vec);
        for v in &mut vec {
            *v = v.clone() + "_test";
        }
        println!("vec={:?}", vec);
    }

    {
        struct T(String);
        struct U(String, Box<usize>);

        let t1 = T("fun".to_string());
        fn f1(T(s): &T) {
            println!("{:?}", s);
            //     let _t = &T(*s);
        }
        f1(&t1);

        let t2 = U("fun".to_string(), Box::new(42));
        fn f2(U(s, v): &U) {
            println!("{:?} {:?}", s, v);
            //     let _t = &T(*s);
        }
        f2(&t2);
    }
    {
        use ref_cast::RefCast;

        #[derive(RefCast)]
        #[repr(transparent)]
        struct U(String);

        // Safely cast from `&String` to `&U`.

        fn f1(U(s): &U) {
            println!("U(s) = U({})", s);
        }
        fn f2(U(s): &U) {
            let u = U::ref_cast(s);
            f1(u);
        }
        f2(&U("test1".to_string()));
    }
}
