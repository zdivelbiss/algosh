var add_one: { (a: Int) => a + 1 }
var add_one_two: {
    (a: Int, b: Int) =>
    a: a + 1;
    b: b + 1;
    (a, b)
}

1 => add_one => (a, 1) => add_one_two