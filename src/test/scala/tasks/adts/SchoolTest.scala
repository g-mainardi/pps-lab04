package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.extensionmethods.Sequences.*
import Sequence.{cons, nil}


class SchoolTest:

  val schoolADT: SchoolModule = BasicSchoolModule
  import schoolADT.*

  @Test def testTeacher(): Unit =
    assertEquals("John", teacher("John"))

  @Test def testCourse(): Unit =
    assertEquals("Math", course("Math"))

  @Test def testEmptySchool(): Unit =
    assertEquals(nil(), emptySchool)

  @Test def testGetCourses(): Unit =
    val noCourses = emptySchool.courses()
    val oneCourse = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math")).courses()
    val twoCourses = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian")).courses()
    val duplicatedCourses = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("Peter"), course("Math")).courses()
    assertEquals(nil(), noCourses)
    assertEquals(cons("Math", nil()), oneCourse)
    assertEquals(cons("Math", nil()), duplicatedCourses)

  @Test def testGetTeachers(): Unit =
    val noTeachers = emptySchool.teachers()
    val oneTeacher = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math")).teachers()
    val duplicatedTeacher = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian")).teachers()
    assertEquals(nil(), noTeachers)
    assertEquals(cons("John", nil()), oneTeacher)
    assertEquals(cons("John", nil()), duplicatedTeacher)

  @Test def testSetTeacherToCourse(): Unit =
    val mySchool = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
    assertEquals(cons("Math", nil()), mySchool.courses())
    assertEquals(cons("John", nil()), mySchool.teachers())

  @Test def testCoursesOfATeacher(): Unit =
    val coursesSchool1 = emptySchool
      .coursesOfATeacher(teacher("John"))
    val coursesSchool2 = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .coursesOfATeacher(teacher("John"))
    val coursesSchool3 = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("John"), course("Italian"))
      .coursesOfATeacher(teacher("John"))
    assertEquals(nil(), coursesSchool1)
    assertEquals(cons("Math", nil()), coursesSchool2)
    assertEquals(cons("Math", cons("Italian", nil())), coursesSchool3)

  @Test def testHasTeacher(): Unit =
    val school1HasJohn = emptySchool
      .hasTeacher("John")
    val school2HasJohn = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .hasTeacher("John")
    assertFalse(school1HasJohn)
    assertTrue(school2HasJohn)

  @Test def testHasCourse(): Unit =
    val school1HasMath = emptySchool
      .hasCourse("Math")
    val school2HasMath = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .hasCourse("Math")
    assertFalse(school1HasMath)
    assertTrue(school2HasMath)
