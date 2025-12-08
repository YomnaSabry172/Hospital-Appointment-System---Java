package hospitalAppointmentSystem;

import java.util.ArrayList;
import java.util.List;

public class HospitalSystem {
    private ArrayList<Doctor> doctors;
    private ArrayList<Patient> patients;
    private ArrayList<Appointment> appointments;

    public HospitalSystem() {
        this.doctors = new ArrayList<>();
        this.patients = new ArrayList<>();
        this.appointments = new ArrayList<>();
    }

    public void addDoctor(Doctor doctor) {
        this.doctors.add(doctor);
    }

    public void addPatient(Patient patient) {
        this.patients.add(patient);
    }

    public void addAppointment(Appointment appointment) {
        this.appointments.add(appointment);
    }

    public void removeDoctor(Doctor doctor) {
        this.doctors.remove(doctor);
    }

    public void removePatient(Patient patient) {
        this.patients.remove(patient);
    }

    public void removeAppointment(Appointment appointment) {
        this.appointments.remove(appointment);
    }

    public Doctor getDoctor(int id) {
        for (Doctor doctor : doctors) {
            if (doctor.getId() == id) {
                return doctor;
            }
        }
        return null;
    }

    public Patient getPatient(int id) {
        for (Patient patient : patients) {
            if (patient.getId() == id) {
                return patient;
            }
        }
        return null;
    }

    public Appointment getAppointment(int id) {
        for (Appointment appointment : appointments) {
            if (appointment.getId() == id) {
                return appointment;
            }
        }
        return null;
    }

    public Doctor editDoctor(int id, String name, String contactInfo,
                             String specialization, String timeSlot) {
        Doctor doctor = getDoctor(id);
        if (doctor != null) {
            doctor.setName(name);
            doctor.setContactInfo(contactInfo);
            doctor.setSpecialization(specialization);
            doctor.setTimeSlot(timeSlot);
            return doctor;
        }
        return null;
    }

    public Patient editPatient(int id, String name, String contactInfo,
                               int medicalHistoryId) {
        Patient patient = getPatient(id);
        if (patient != null) {
            patient.setName(name);
            patient.setContactInfo(contactInfo);
            patient.setMedicalHistoryId(medicalHistoryId);
            return patient;
        }
        return null;
    }

    public Appointment editAppointment(int id, String date, Boolean status) {
        Appointment appointment = getAppointment(id);
        if (appointment != null) {
            appointment.setDate(date);
            appointment.setStatus(status);
            return appointment;
        }
        return null;
    }

    // create ONE free slot if there is no identical slot already
    public Appointment addAvailableSlot(int doctorId, String date, String time) {
        Doctor doctor = getDoctor(doctorId);
        if (doctor == null) {
            return null;
        }

        for (Appointment appointment : this.appointments) {
            if (appointment.getDoctorId() == doctorId
                && appointment.getDate().equals(date)
                && appointment.getTime().equals(time)) {
                return null;
            }
        }

        Appointment appointment = new Appointment(doctorId, null, date, time);
        this.appointments.add(appointment);
        return appointment;
    }

    public boolean bookAppointment(int doctorId, String date, String time, int patientId) {
        Doctor doctor = getDoctor(doctorId);
        Patient patient = getPatient(patientId);
        if (doctor == null || patient == null) {
            return false;
        }
        for (Appointment appointment : this.appointments) {
            if (appointment.getDoctorId() == doctorId
                && appointment.getDate().equals(date)
                && appointment.getTime().equals(time)
                && Boolean.FALSE.equals(appointment.getStatus())) {
                appointment.setPatientId(patientId);
                return true;
            }
        }
        return false;
    }

    public boolean cancelAppointment(int doctorId, String date, String time) {
        Doctor doctor = getDoctor(doctorId);
        if (doctor == null) {
            return false;
        }
        for (Appointment appointment : this.appointments) {
            if (appointment.getDoctorId() == doctorId
                && appointment.getDate().equals(date)
                && appointment.getTime().equals(time)
                && Boolean.TRUE.equals(appointment.getStatus())) {
                appointment.setPatientId(null);
                appointment.setStatus(false);
                return true;
            }
        }
        return false;
    }

    public ArrayList<Appointment> getBookedAppointmentsByDoctor(Doctor doctor) {
        ArrayList<Appointment> result = new ArrayList<>();
        for (Appointment appointment : this.appointments) {
            if (appointment.getDoctorId() == doctor.getId()
                && Boolean.TRUE.equals(appointment.getStatus())) {
                result.add(appointment);
            }
        }
        return result;
    }

    public ArrayList<Appointment> getAvailableAppointmentsByDoctor(Doctor doctor) {
        ArrayList<Appointment> result = new ArrayList<>();
        for (Appointment appointment : this.appointments) {
            if (appointment.getDoctorId() == doctor.getId()
                && Boolean.FALSE.equals(appointment.getStatus())) {
                result.add(appointment);
            }
        }
        return result;
    }

    public void generateAvailableAppointments(Doctor doctor, String date) {
        for (Appointment apt : appointments) {
            if (apt.getDoctorId() == doctor.getId()
                && apt.getDate().equals(date)) {
                return;
            }
        }

        String[] times = doctor.getTimeSlot().split("-");
        String startTime = times[0];
        String endTime   = times[1];

        int startHour = Integer.parseInt(startTime.split(":")[0]);
        int startMin  = Integer.parseInt(startTime.split(":")[1]);
        int endHour   = Integer.parseInt(endTime.split(":")[0]);
        int endMin    = Integer.parseInt(endTime.split(":")[1]);

        int currentHour = startHour;
        int currentMin  = startMin;

        while (currentHour < endHour ||
              (currentHour == endHour && currentMin < endMin)) {

            String timeSlot = String.format("%02d:%02d", currentHour, currentMin);

            Appointment appointment = new Appointment(doctor.getId(), null, date, timeSlot);
            this.appointments.add(appointment);

            currentMin += 30;
            if (currentMin >= 60) {
                currentMin = 0;
                currentHour++;
            }
        }
    }

    public ArrayList<Doctor> getDoctors() {
        return doctors;
    }

    public ArrayList<Patient> getPatients() {
        return patients;
    }

    public ArrayList<Appointment> getAppointments() {
        return appointments;
    }
}
