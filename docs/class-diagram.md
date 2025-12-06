# UML Class Diagram – Java Hospital Appointment System

The diagram below shows the main classes and how they are related.

```mermaid
classDiagram

  class Person {
    - int id
    - String name
    ..other shared fields..
  }

  class Doctor {
    - String specialization
    ..other doctor fields..
  }

  class Patient {
    - String phone
    - String address
    ..other patient fields..
  }

  class Appointment {
    - LocalDate date
    - LocalTime time
    ..other appointment fields..
  }

  class HospitalSystem {
    - List~Doctor~ doctors
    - List~Patient~ patients
    - List~Appointment~ appointments
    + addDoctor(Doctor)
    + addPatient(Patient)
    + addAppointment(Appointment)
    + getDoctors() List~Doctor~
    + getPatients() List~Patient~
    + getAppointments() List~Appointment~
  }

  class MainApp
  class Main
  class DoctorView
  class PatientView
  class AppointmentView
  class UIUtils

  Person <|-- Doctor
  Person <|-- Patient

  Doctor "1" -- "0..*" Appointment
  Patient "1" -- "0..*" Appointment

  HospitalSystem "1" o-- "0..*" Doctor
  HospitalSystem "1" o-- "0..*" Patient
  HospitalSystem "1" o-- "0..*" Appointment

  MainApp --> HospitalSystem
  MainApp --> DoctorView
  MainApp --> PatientView
  MainApp --> AppointmentView

  DoctorView --> HospitalSystem
  PatientView --> HospitalSystem
  AppointmentView --> HospitalSystem

  DoctorView ..> UIUtils
  PatientView ..> UIUtils
  AppointmentView ..> UIUtils
