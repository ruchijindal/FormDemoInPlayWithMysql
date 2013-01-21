package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class Employee(id: Pk[Int] = NotAssigned, email: String, password: String)

object Employee {

  /**
   * Parse a Employee from a ResultSet
   */
  val simple = {
    get[Pk[Int]]("employee.employee_id") ~
      get[String]("employee.email") ~
      get[String]("employee.password") map {
        case id ~ email ~ password => Employee(id, email, password)
      }
  }

  /**
   * Register a new employee.
   *
   * @param employee The computer values.
   */
  def insert(employee: Employee): Int = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into EMPLOYEE(EMAIL,PASSWORD) values (
            {email}, {password}
          )
        """).on(
          'email -> employee.email,
          'password -> employee.password).executeUpdate()
    }
  }

  /**
   * Find Employee Via Email Id
   *
   * @param email the employee email id.
   */
  def findByEmployeeEmail(email: String) = {
    DB.withConnection { implicit connection =>
      val employees = SQL(
        """
          select * from EMPLOYEE 
          where EMAIL = {email}
        """).on(
          'email -> email).as(Employee.simple.*)
      employees
    }
  }
  /**
   * Find Employee Via Employee Id
   *
   * @param id the employee  id.
   */
  def findByEmployeeId(id: Int) = {
    DB.withConnection { implicit connection =>
      val employeeFound = SQL(
        """
          select * from EMPLOYEE 
          where EMPLOYEE_ID = {id}
        """).on(
          'id -> id).as(Employee.simple.singleOpt)
      employeeFound
    }
  }

  /**
   * Find Max Employee Id
   */
  def findMaxEmployeeId = {
    DB.withConnection { implicit connection =>
      val empId = SQL(
        """
          select MAX(EMPLOYEE_ID) from EMPLOYEE 
        """).as(scalar[Int].single)
      empId
    }
  }

  /**
   * Find Employee Via Email and password
   */
  def authenticate(employee: Employee) = {
    DB.withConnection { implicit connection =>
      val employeeFound = SQL(
        """
          select * from EMPLOYEE 
          where EMAIL = {email} and PASSWORD= {password}
        """).on(
          'email -> employee.email,
          'password -> employee.password).as(Employee.simple.singleOpt)
      employeeFound
    }
  }

  /**
   * Delete All Employees.
   */
  def delete = {
    DB.withConnection { implicit connection =>
      SQL("delete from EMPLOYEE").executeUpdate()
    }
  }

}