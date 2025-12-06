# Java Hospital Appointment System

## 1. Project Overview
This is a JavaFX-based Hospital Appointment Management System.  
It provides a simple graphical interface for managing doctors, patients, and their appointments in a small clinic or hospital.

The application is mainly intended as an educational project to demonstrate:
- Object-Oriented Programming (OOP) in Java
- Separation between data model and user interface
- Basic GUI design using JavaFX

---

## 2. Main Features
- **Doctor Management**
  - Add new doctors with basic information (e.g. ID, name, specialization).
  - View the list of all registered doctors.

- **Patient Management**
  - Register new patients with their personal information.
  - View the list of existing patients.

- **Appointment Management**
  - Create appointments by linking a doctor with a patient and assigning date/time.
  - View the list of all appointments.

- **Tabbed User Interface**
  - JavaFX `TabPane` with separate tabs:
    - Doctors tab
    - Patients tab
    - Appointments tab

---

## 3. Technologies Used
- **Programming Language:** Java
- **GUI Framework:** JavaFX
- **IDE:** Eclipse (or IntelliJ IDEA – specify what you used)
- **Build Type:** Standard Java project (no external build tool)  
  *(If you use Maven/Gradle, explain here.)*

---

## 4. Project Structure

## 4. Project Structure

```text
src/
  hospitalAppointmentSystem/
    Appointment.java          // Appointment entity class
    AppointmentView.java      // JavaFX UI for appointment management
    Doctor.java               // Doctor entity class (extends Person if implemented)
    DoctorView.java           // JavaFX UI for doctor management
    HospitalSystem.java       // Core logic: manages doctors, patients, and appointments
    Main.java                 // (If used) a simple launcher or utility main class
    MainApp.java              // Main JavaFX application entry point
    Patient.java              // Patient entity class (extends Person if implemented)
    PatientView.java          // JavaFX UI for patient management
    Person.java               // Base class for Doctor and Patient
    UIUtils.java              // Shared UI utilities (FXML helpers, alerts, styling)
    style.css                 // CSS styling for the JavaFX UI
