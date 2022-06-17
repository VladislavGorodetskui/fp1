package com.korolyagaylo.fplab1

import com.korolyagaylo.fplab1
import munit.FunSuite
import MyList.*

class MyListSuite extends FunSuite {
  test("spans  []") {
    val expected = MyNil
    val actual = spans(MyNil)
    assertEquals(actual, expected)
  }

  test("spans for [1,1,2,3,4,4,1,2]") {
    val expected = MyList(MyList(1,1),MyList(2),MyList(3),MyList(4,4),MyList(1),MyList(2))
    val actual = spans(MyList(1,1,2,3,4,4,1,2))
    assertEquals(actual, expected)
  }

  test("spans for [1, 2, 2, 2, 3, 2, 2, 1]") {
    val expected = MyList(MyList(1),MyList(2, 2, 2),MyList(3),MyList(2, 2),MyList(1))
    val actual = spans(MyList(1, 2, 2, 2, 3, 2, 2, 1))
    assertEquals(actual, expected)
  }

  test("groupBy for []") {
    val expected : Map[Any, MyList[Any]] = Map.empty
    val actual = groupBy(MyNil, x => x)
    assertEquals(actual, expected)
  }

  test("groupBy for [1, 2, 3, 4, 5, 6, 7, 8, 9] and  % 2") {
    val expected = Map(1 -> MyList(1, 3, 5, 7, 9), 0 -> MyList(2, 4, 6, 8))
    val actual = groupBy(MyList(1,2,3,4,5,6,7,8,9), x => x % 2)
    assertEquals(actual, expected)
  }
}