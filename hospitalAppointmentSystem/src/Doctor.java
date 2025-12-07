package hospitalAppointmentSystem;

public class Doctor extends Person {
    private String specialization;
    private String timeSlot;  // e.g., "09:00-17:00"
    private static int doctorCounter = 1;

    public Doctor(String name, String contactInfo, String nationalId,
            String specialization, String timeSlot) {
  super(name, contactInfo, nationalId, doctorCounter++);
  this.specialization = specialization;
  this.timeSlot = timeSlot;
}

    public String getSpecialization() {
        return specialization;
    }

    public String getTimeSlot() {
        return timeSlot;
    }

    public void setSpecialization(String specialization) {
        this.specialization = specialization;
    }

    public void setTimeSlot(String timeSlot) {
        this.timeSlot = timeSlot;
    }
    @Override
    public String toString() {
        return getName() + " (ID " + getId() + ")";
    }
    @Override
public String getRole() {
    return "Doctor";
    }
}

