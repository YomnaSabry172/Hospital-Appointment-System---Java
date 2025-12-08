package hospitalAppointmentSystem;

public class Appointment {

    private int doctorId;
    private Integer patientId;   // null = free slot
    private String date;
    private String time;
    private Boolean status;
    private int id;
    private static int idCounter = 1;

    public Appointment(int doctorId, Integer patientId, String date, String time) {
        this.doctorId = doctorId;
        this.patientId = patientId;   // null means free
        this.date = date;
        this.time = time;
        this.status = (patientId != null);
        this.id = idCounter++;
    }

    public int getDoctorId() {
        return doctorId;
    }

    public Integer getPatientId() {
        return patientId;
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

    public void setPatientId(Integer patientId) {
        this.patientId = patientId;
        this.status = (patientId != null);
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
        String p = (patientId == null) ? "Available" : ("Patient ID " + patientId);
        return "Appointment ID " + id + " | Doctor ID " + doctorId + " | " +
               date + " " + time + " | " + p + " | " + (status ? "Booked" : "Free");
    }
}
