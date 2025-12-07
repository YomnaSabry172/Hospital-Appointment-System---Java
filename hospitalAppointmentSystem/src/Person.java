package hospitalAppointmentSystem;

public abstract  class Person {
    private String name;
    private int id;
    private String nationalId;
    private String contactInfo;

    public Person(String name, String contactInfo, String nationalId, int id) {
        this.name = name;
        this.contactInfo = contactInfo;
        this.nationalId = nationalId;
        this.id = id;  // ID is now passed from subclass
    }

    public String getName() {
        return name;
    }

    public int getId() {
        return id;
    }

    public String getContactInfo() {
        return contactInfo;
    }

    public String getNationalId() {
        return nationalId;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setContactInfo(String contactInfo) {
        this.contactInfo = contactInfo;
    }

    public void setNationalId(String nationalId) {
        this.nationalId = nationalId;
    }
}

