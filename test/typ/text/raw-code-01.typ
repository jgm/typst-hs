#set page(width: 180pt)
#set text(6pt)

```rust
/// A carefully designed state machine.
#[derive(Debug)]
enum State<'a> { A(u8), B(&'a str) }

fn advance(state: State<'_>) -> State<'_> {
    unimplemented!("state machine")
}
```

