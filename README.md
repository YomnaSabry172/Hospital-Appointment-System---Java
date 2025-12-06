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
    style.css                 // CSS styling for the JavaFX UI ...
```
## 5. How the System Works (Design Explanation)

### **Model Layer (Entities)**
- **Person**  
  Base class containing shared fields such as `id` and `name`.  
- **Doctor**  
  Extends `Person`; includes doctor-specific fields like specialization.  
- **Patient**  
  Extends `Person`; includes patient-specific information such as phone or address.  
- **Appointment**  
  Represents a meeting between a doctor and a patient at a specific date and time.

These model classes hold data and simple operations (constructors, getters, setters).

---

### **Logic Layer (HospitalSystem)**
`HospitalSystem` acts as the core logic of the application.  
It stores all doctors, patients, and appointments in lists and provides operations such as:

- `addDoctor(Doctor doctor)`  
- `addPatient(Patient patient)`  
- `addAppointment(Appointment appointment)`  
- `getDoctors()` → `List<Doctor>`  
- `getPatients()` → `List<Patient>`  
- `getAppointments()` → `List<Appointment>`  

It works as an **in-memory database** shared by all UI components.

---

### **UI Layer (JavaFX Views)**
These classes create the visual interface and interact with the logic layer:

- **DoctorView** – handles doctor form + table  
- **PatientView** – handles patient form + table  
- **AppointmentView** – handles linking doctor + patient + date/time  

Each view:
1. Receives the shared `HospitalSystem` instance  
2. Builds JavaFX UI controls (buttons, fields, tables)  
3. Calls system methods like `addDoctor()` or `addAppointment()`  

---

### **Application Layer (MainApp)**
- Creates a single `HospitalSystem` object  
- Creates and loads all three View tabs  
- Places them into a `TabPane`  
- Starts the JavaFX application window  

`MainApp` is the true starting point of the program.

---

## 6. How to Run the Project

### **1. Clone or Download**
```bash
git clone https://github.com/<YomnaSabry172>/Hospital-Appointment-System---Java.git


