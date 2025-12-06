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

public class PatientView {

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

        Label historyLabel = new Label("Medical History ID:");
        TextField historyField = new TextField();

        form.addRow(0, nameLabel,       nameField);
        form.addRow(1, contactLabel,    contactField);
        form.addRow(2, nationalIdLabel, nationalIdField);
        form.addRow(3, historyLabel,    historyField);

        // ---------- 2) Buttons row ----------

        Button addButton    = new Button("Add Patient");
        Button updateButton = new Button("Update Patient");
        Button removeButton = new Button("Remove Patient");

        HBox buttonRow = new HBox(10, addButton, updateButton, removeButton);

        // ---------- 3) TableView ----------

        TableView<Patient> table = new TableView<>();
        table.setPrefHeight(250);

        TableColumn<Patient, Integer> idCol = new TableColumn<>("ID");
        idCol.setCellValueFactory(new PropertyValueFactory<>("id"));
        idCol.setPrefWidth(60);

        TableColumn<Patient, String> nameCol = new TableColumn<>("Name");
        nameCol.setCellValueFactory(new PropertyValueFactory<>("name"));
        nameCol.setPrefWidth(150);

        TableColumn<Patient, Integer> histCol = new TableColumn<>("Medical History ID");
        histCol.setCellValueFactory(new PropertyValueFactory<>("medicalHistoryId"));
        histCol.setPrefWidth(160);

        table.getColumns().addAll(idCol, nameCol, histCol);

        ObservableList<Patient> patientData =
                FXCollections.observableArrayList(hospitalSystem.getPatients());
        table.setItems(patientData);

        // ---------- 4) Log area ----------

        TextArea outputArea = new TextArea();
        outputArea.setEditable(false);
        outputArea.setPrefRowCount(5);

        // ---------- 5) Selection → form ----------

        table.getSelectionModel().selectedItemProperty().addListener((obs, oldP, newP) -> {
            if (newP != null) {
                nameField.setText(newP.getName());
                contactField.setText(newP.getContactInfo());
                nationalIdField.setText(newP.getNationalId());
                historyField.setText(String.valueOf(newP.getMedicalHistoryId()));
            }
        });

        // ---------- 6) Button actions ----------

        // Add
        addButton.setOnAction(e -> {
            try {
                String name       = nameField.getText().trim();
                String contact    = contactField.getText().trim();
                String nationalId = nationalIdField.getText().trim();
                String historyStr = historyField.getText().trim();

                if (name.isEmpty() || contact.isEmpty() || nationalId.isEmpty() || historyStr.isEmpty()) {
                    UiUtils.showError("Invalid input", "Please fill all patient fields.");
                    outputArea.appendText("Error: some patient fields are empty.\n");
                    return;
                }

                int historyId = Integer.parseInt(historyStr);

                Patient p = new Patient(name, contact, nationalId, historyId);
                hospitalSystem.addPatient(p);
                patientData.add(p);

                outputArea.appendText("✓ Patient added: " + name +
                                      " (ID = " + p.getId() + ")\n");
                UiUtils.showInfo("Patient added",
                        "Patient " + name + " was added with ID " + p.getId() + ".");
                clearForm(nameField, contactField, nationalIdField, historyField);

            } catch (NumberFormatException ex) {
                UiUtils.showError("Invalid number", "Medical History ID must be a number.");
                outputArea.appendText("Error: Medical History ID not numeric.\n");
            }
        });

        // Update
        updateButton.setOnAction(e -> {
            Patient selected = table.getSelectionModel().getSelectedItem();
            if (selected == null) {
                UiUtils.showError("No selection", "Select a patient from the table to update.");
                outputArea.appendText("Error: no patient selected for update.\n");
                return;
            }

            try {
                String name       = nameField.getText().trim();
                String contact    = contactField.getText().trim();
                String nationalId = nationalIdField.getText().trim();
                String historyStr = historyField.getText().trim();

                if (name.isEmpty() || contact.isEmpty() || nationalId.isEmpty() || historyStr.isEmpty()) {
                    UiUtils.showError("Invalid input", "Please fill all patient fields before updating.");
                    outputArea.appendText("Error: incomplete patient fields for update.\n");
                    return;
                }

                int historyId = Integer.parseInt(historyStr);

                hospitalSystem.editPatient(
                        selected.getId(), name, contact, historyId
                );
                selected.setNationalId(nationalId);

                table.refresh();
                outputArea.appendText("✓ Patient updated: ID " + selected.getId() + ".\n");
                UiUtils.showInfo("Patient updated",
                        "Patient with ID " + selected.getId() + " has been updated.");

            } catch (NumberFormatException ex) {
                UiUtils.showError("Invalid number", "Medical History ID must be a number.");
                outputArea.appendText("Error: Medical History ID not numeric.\n");
            }
        });

        // Remove
        removeButton.setOnAction(e -> {
            Patient selected = table.getSelectionModel().getSelectedItem();
            if (selected == null) {
                UiUtils.showError("No selection", "Select a patient from the table to remove.");
                outputArea.appendText("Error: no patient selected for removal.\n");
                return;
            }

            boolean ok = UiUtils.confirm(
                    "Confirm removal",
                    "Are you sure you want to remove patient ID " + selected.getId() +
                    " (" + selected.getName() + ")?"
            );
            if (!ok) {
                return;
            }

            hospitalSystem.removePatient(selected);
            patientData.remove(selected);

            outputArea.appendText("✓ Patient removed: ID " + selected.getId() + ".\n");
            UiUtils.showInfo("Patient removed",
                    "Patient ID " + selected.getId() + " has been removed.");
            clearForm(nameField, contactField, nationalIdField, historyField);
        });

        // ---------- 7) Layout ----------

        root.getChildren().addAll(form, buttonRow, table, outputArea);
        return root;
    }

    private static void clearForm(TextField nameField,
                                  TextField contactField,
                                  TextField nationalIdField,
                                  TextField historyField) {
        nameField.clear();
        contactField.clear();
        nationalIdField.clear();
        historyField.clear();
    }
}
