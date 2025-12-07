package hospitalAppointmentSystem;

public class Patient extends Person {
    private int medicalHistoryId;
    private static int patientCounter = 1;

    public Patient(String name, String contactInfo, String nationalId,
            int medicalHistoryId) {
    	super(name, contactInfo, nationalId, patientCounter++);
    	this.medicalHistoryId = medicalHistoryId;
}


    public int getMedicalHistoryId() {
        return medicalHistoryId;
    }

    public void setMedicalHistoryId(int medicalHistoryId) {
        this.medicalHistoryId = medicalHistoryId;
    }
    @Override
    public String toString() {
        return getName() + " (ID " + getId() + ")";
    }
    @Override
public String getRole() {
    return "Patient";
    }
}

