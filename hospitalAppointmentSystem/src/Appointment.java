package hospitalAppointmentSystem;

public class Appointment {
    private Patient patient;   // null means available slot
    private Doctor doctor;
    private String date;
    private String time;
    private boolean status;    // true = booked, false = available
    private int id;
    private static int idCounter = 1;

    public Appointment(Patient patient, Doctor doctor, String date, String time) {
        this.patient = patient;
        this.doctor = doctor;
        this.date = date;
        this.time = time;
        this.status = (patient != null);   // booked only if patient is not null
        this.id = generateId();
    }

    private int generateId() {
        return idCounter++;
    }

    public Patient getPatient() {
        return patient;
    }

    public Doctor getDoctor() {
        return doctor;
    }

    public String getDate() {
        return date;
    }

    public String getTime() {
        return time;
    }

    public Boolean getStatus() {
        return status;
    }

    public int getId() {
        return id;
    }

    public void setPatient(Patient patient) {
        this.patient = patient;
        this.status = (patient != null);  // keep status consistent
    }

    public void setDoctor(Doctor doctor) {
        this.doctor = doctor;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public void setTime(String time) {
        this.time = time;
    }

    public void setStatus(Boolean status) {
        this.status = status;
    }
    @Override
    public String toString() {
        String patientName = (patient == null) ? "Available" : patient.getName();
        String statusText  = (status != null && status) ? "Booked" : "Free";

        return String.format("ID %d | %s %s | Doctor: %s | Patient: %s | %s",
                id, date, time, doctor.getName(), patientName, statusText);
    }

}

