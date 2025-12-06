package hospitalAppointmentSystem;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

public class DoctorView {

    public static VBox create(HospitalSystem hospitalSystem) {

        VBox root = new VBox(10);
        root.setPadding(new Insets(10));

        // ---------- 1) Form controls ----------

        GridPane form = new GridPane();
        form.setHgap(10);
        form.setVgap(10);

        Label nameLabel = new Label("Name:");
        TextField nameField = new TextField();

        Label contactLabel = new Label("Contact Info:");
        TextField contactField = new TextField();

        Label nationalIdLabel = new Label("National ID:");
        TextField nationalIdField = new TextField();

        Label specLabel = new Label("Specialization:");
        TextField specField = new TextField();

        Label timeSlotLabel = new Label("Time Slot (e.g. 09:00-17:00):");
        TextField timeSlotField = new TextField();

        form.addRow(0, nameLabel,       nameField);
        form.addRow(1, contactLabel,    contactField);
        form.addRow(2, nationalIdLabel, nationalIdField);
        form.addRow(3, specLabel,       specField);
        form.addRow(4, timeSlotLabel,   timeSlotField);

        // ---------- 2) Buttons row ----------

        Button addButton    = new Button("Add Doctor");
        Button updateButton = new Button("Update Doctor");
        Button removeButton = new Button("Remove Doctor");

        HBox buttonRow = new HBox(10, addButton, updateButton, removeButton);

        // ---------- 3) TableView ----------

        TableView<Doctor> table = new TableView<>();
        table.setPrefHeight(250);

        TableColumn<Doctor, Integer> idCol = new TableColumn<>("ID");
        idCol.setCellValueFactory(new PropertyValueFactory<>("id"));
        idCol.setPrefWidth(60);

        TableColumn<Doctor, String> nameCol = new TableColumn<>("Name");
        nameCol.setCellValueFactory(new PropertyValueFactory<>("name"));
        nameCol.setPrefWidth(150);

        TableColumn<Doctor, String> specCol = new TableColumn<>("Specialization");
        specCol.setCellValueFactory(new PropertyValueFactory<>("specialization"));
        specCol.setPrefWidth(150);

        TableColumn<Doctor, String> timeCol = new TableColumn<>("Time Slot");
        timeCol.setCellValueFactory(new PropertyValueFactory<>("timeSlot"));
        timeCol.setPrefWidth(120);

        table.getColumns().addAll(idCol, nameCol, specCol, timeCol);

        ObservableList<Doctor> doctorData =
                FXCollections.observableArrayList(hospitalSystem.getDoctors());
        table.setItems(doctorData);

        // ---------- 4) Log area ----------

        TextArea outputArea = new TextArea();
        outputArea.setEditable(false);
        outputArea.setPrefRowCount(5);

        // ---------- 5) Table selection → fill form ----------

        table.getSelectionModel().selectedItemProperty().addListener((obs, oldDoc, newDoc) -> {
            if (newDoc != null) {
                nameField.setText(newDoc.getName());
                contactField.setText(newDoc.getContactInfo());
                nationalIdField.setText(newDoc.getNationalId());
                specField.setText(newDoc.getSpecialization());
                timeSlotField.setText(newDoc.getTimeSlot());
            }
        });

        // ---------- 6) Button actions ----------

        // Add
        addButton.setOnAction(e -> {
            String name       = nameField.getText().trim();
            String contact    = contactField.getText().trim();
            String nationalId = nationalIdField.getText().trim();
            String spec       = specField.getText().trim();
            String timeSlot   = timeSlotField.getText().trim();

            if (name.isEmpty() || contact.isEmpty() || nationalId.isEmpty()
                    || spec.isEmpty() || timeSlot.isEmpty()) {
                UiUtils.showError("Invalid input", "Please fill all doctor fields.");
                outputArea.appendText("Error: some doctor fields are empty.\n");
                return;
            }

            Doctor d = new Doctor(name, contact, nationalId, spec, timeSlot);
            hospitalSystem.addDoctor(d);
            doctorData.add(d);

            outputArea.appendText("✓ Doctor added: " + name +
                                  " (ID = " + d.getId() + ")\n");
            UiUtils.showInfo("Doctor added",
                    "Doctor " + name + " was added with ID " + d.getId() + ".");
            clearForm(nameField, contactField, nationalIdField, specField, timeSlotField);
        });

        // Update
        updateButton.setOnAction(e -> {
            Doctor selected = table.getSelectionModel().getSelectedItem();
            if (selected == null) {
                UiUtils.showError("No selection", "Select a doctor from the table to update.");
                outputArea.appendText("Error: no doctor selected for update.\n");
                return;
            }

            String name       = nameField.getText().trim();
            String contact    = contactField.getText().trim();
            String nationalId = nationalIdField.getText().trim();
            String spec       = specField.getText().trim();
            String timeSlot   = timeSlotField.getText().trim();

            if (name.isEmpty() || contact.isEmpty() || nationalId.isEmpty()
                    || spec.isEmpty() || timeSlot.isEmpty()) {
                UiUtils.showError("Invalid input", "Please fill all doctor fields before updating.");
                outputArea.appendText("Error: incomplete fields for update.\n");
                return;
            }

            hospitalSystem.editDoctor(
                    selected.getId(), name, contact, spec, timeSlot
            );
            selected.setNationalId(nationalId);

            table.refresh();

            outputArea.appendText("✓ Doctor updated: ID " + selected.getId() + ".\n");
            UiUtils.showInfo("Doctor updated",
                    "Doctor with ID " + selected.getId() + " has been updated.");
        });

        // Remove
        removeButton.setOnAction(e -> {
            Doctor selected = table.getSelectionModel().getSelectedItem();
            if (selected == null) {
                UiUtils.showError("No selection", "Select a doctor from the table to remove.");
                outputArea.appendText("Error: no doctor selected for removal.\n");
                return;
            }

            boolean ok = UiUtils.confirm(
                    "Confirm removal",
                    "Are you sure you want to remove doctor ID " + selected.getId() +
                    " (" + selected.getName() + ")?"
            );
            if (!ok) {
                return;
            }

            hospitalSystem.removeDoctor(selected);
            doctorData.remove(selected);

            outputArea.appendText("✓ Doctor removed: ID " + selected.getId() + ".\n");
            UiUtils.showInfo("Doctor removed",
                    "Doctor ID " + selected.getId() + " has been removed.");
            clearForm(nameField, contactField, nationalIdField, specField, timeSlotField);
        });

        // ---------- 7) Layout ----------

        root.getChildren().addAll(form, buttonRow, table, outputArea);
        return root;
    }

    private static void clearForm(TextField nameField,
                                  TextField contactField,
                                  TextField nationalIdField,
                                  TextField specField,
                                  TextField timeSlotField) {
        nameField.clear();
        contactField.clear();
        nationalIdField.clear();
        specField.clear();
        timeSlotField.clear();
    }
    
}
