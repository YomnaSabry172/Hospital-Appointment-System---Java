package hospitalAppointmentSystem;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

public class AppointmentView {

    public static BorderPane create(HospitalSystem hospitalSystem) {

        BorderPane root = new BorderPane();
        root.setPadding(new Insets(10));

        // ---------- TOP: doctor + date + refresh/generate ----------

        GridPane topForm = new GridPane();
        topForm.setHgap(10);
        topForm.setVgap(10);

        Label doctorLabel = new Label("Doctor:");
        ComboBox<Doctor> doctorCombo = new ComboBox<>();
        doctorCombo.setPromptText("Select doctor...");

        Label dateLabel = new Label("Date:");
        DatePicker datePicker = new DatePicker(LocalDate.now());

        Button refreshButton = new Button("Refresh Doctors / Patients");
        Button generateButton = new Button("Generate Slots");

        topForm.addRow(0, doctorLabel, doctorCombo);
        topForm.addRow(1, dateLabel, datePicker);
        topForm.add(refreshButton, 0, 2, 2, 1);
        topForm.add(generateButton, 2, 2);

        // ---------- CENTER: header + table + summary ----------

        Label headerLabel = new Label("No doctor/date selected");
        Label summaryLabel = new Label("Summary: 0 total | 0 booked | 0 free");

        TableView<Appointment> table = new TableView<>();
        table.setPrefHeight(260);

        TableColumn<Appointment, Integer> idCol = new TableColumn<>("ID");
        idCol.setCellValueFactory(new PropertyValueFactory<>("id"));
        idCol.setPrefWidth(60);

        TableColumn<Appointment, String> timeCol = new TableColumn<>("Time");
        timeCol.setCellValueFactory(new PropertyValueFactory<>("time"));
        timeCol.setPrefWidth(90);

        TableColumn<Appointment, String> patientCol = new TableColumn<>("Patient");
        patientCol.setCellValueFactory(cellData -> {
            Patient p = cellData.getValue().getPatient();
            String name = (p == null) ? "Available" : p.getName();
            return new SimpleStringProperty(name);
        });
        patientCol.setPrefWidth(150);

        TableColumn<Appointment, String> statusCol = new TableColumn<>("Status");
        statusCol.setCellValueFactory(cellData -> {
            Boolean status = cellData.getValue().getStatus();
            String text = (status != null && status) ? "Booked" : "Free";
            return new SimpleStringProperty(text);
        });
        statusCol.setPrefWidth(80);

        table.getColumns().addAll(idCol, timeCol, patientCol, statusCol);

        ObservableList<Appointment> displayedAppointments =
                FXCollections.observableArrayList();
        table.setItems(displayedAppointments);

        VBox centerBox = new VBox(8, headerLabel, table, summaryLabel);
        centerBox.setPadding(new Insets(10, 0, 0, 0));

        // ---------- BOTTOM: patient + actions + log ----------

        VBox bottomBox = new VBox(10);
        bottomBox.setPadding(new Insets(10, 0, 0, 0));

        GridPane bottomForm = new GridPane();
        bottomForm.setHgap(10);
        bottomForm.setVgap(10);

        Label patientLabel = new Label("Patient:");
        ComboBox<Patient> patientCombo = new ComboBox<>();
        patientCombo.setPromptText("Select patient...");

        bottomForm.addRow(0, patientLabel, patientCombo);

        Button bookButton = new Button("Book Selected Slot");
        Button cancelButton = new Button("Cancel Selected Appointment");
        Button showBookedButton = new Button("Show Booked");
        Button showAvailableButton = new Button("Show Available");

        HBox buttonRow = new HBox(10, bookButton, cancelButton,
                                  showBookedButton, showAvailableButton);

        TextArea logArea = new TextArea();
        logArea.setEditable(false);
        logArea.setPrefRowCount(5);

        bottomBox.getChildren().addAll(bottomForm, buttonRow, logArea);

        // ---------- Place into root ----------

        root.setTop(topForm);
        root.setCenter(centerBox);
        root.setBottom(bottomBox);

        // ---------- helper runnables ----------

        Runnable refreshCombos = () -> {
            doctorCombo.getItems().setAll(hospitalSystem.getDoctors());
            patientCombo.getItems().setAll(hospitalSystem.getPatients());

            if (!doctorCombo.getItems().isEmpty()
                    && doctorCombo.getValue() == null) {
                doctorCombo.getSelectionModel().selectFirst();
            }
            if (!patientCombo.getItems().isEmpty()
                    && patientCombo.getValue() == null) {
                patientCombo.getSelectionModel().selectFirst();
            }
        };

        Runnable updateHeaderAndSummary = () -> {
            Doctor d = doctorCombo.getValue();
            LocalDate date = datePicker.getValue();

            if (d == null || date == null) {
                headerLabel.setText("No doctor/date selected");
                summaryLabel.setText("Summary: 0 total | 0 booked | 0 free");
                return;
            }

            String dateStr = date.toString();
            headerLabel.setText("Appointments for Dr. " + d.getName()
                                + " on " + dateStr);

            int total = displayedAppointments.size();
            long booked = displayedAppointments.stream()
                    .filter(a -> Boolean.TRUE.equals(a.getStatus()))
                    .count();
            long free = total - booked;

            summaryLabel.setText("Summary: " + total + " total | "
                                 + booked + " booked | "
                                 + free + " free");
        };

        Runnable refreshTable = () -> {
            Doctor selectedDoctor = doctorCombo.getValue();
            LocalDate pickedDate = datePicker.getValue();
            if (selectedDoctor == null || pickedDate == null) {
                displayedAppointments.clear();
                updateHeaderAndSummary.run();
                return;
            }
            String dateStr = pickedDate.toString();

            List<Appointment> filtered = hospitalSystem.getAppointments()
                    .stream()
                    .filter(a -> a.getDoctor().getId() == selectedDoctor.getId()
                              && a.getDate().equals(dateStr))
                    .collect(Collectors.toList());

            displayedAppointments.setAll(filtered);
            updateHeaderAndSummary.run();
        };

        Runnable updateButtonsState = () -> {
            boolean hasDoctor = doctorCombo.getValue() != null;
            boolean hasDate = datePicker.getValue() != null;
            boolean hasDoctorAndDate = hasDoctor && hasDate;

            boolean hasRows = !displayedAppointments.isEmpty();
            boolean hasSelection = table.getSelectionModel().getSelectedItem() != null;
            boolean hasPatient = patientCombo.getValue() != null;

            generateButton.setDisable(!hasDoctorAndDate);
            showBookedButton.setDisable(!hasDoctorAndDate || !hasRows);
            showAvailableButton.setDisable(!hasDoctorAndDate || !hasRows);
            bookButton.setDisable(!(hasSelection && hasPatient));
            cancelButton.setDisable(!hasSelection);
        };

        // ---------- listeners ----------

        doctorCombo.valueProperty().addListener((obs, oldVal, newVal) -> {
            refreshTable.run();
            updateButtonsState.run();
        });

        datePicker.valueProperty().addListener((obs, oldVal, newVal) -> {
            refreshTable.run();
            updateButtonsState.run();
        });

        patientCombo.valueProperty().addListener((obs, oldVal, newVal) -> {
            updateButtonsState.run();
        });

        table.getSelectionModel().selectedItemProperty().addListener((obs, oldSel, newSel) -> {
            updateButtonsState.run();
        });

        // ---------- Button actions ----------

        refreshButton.setOnAction(e -> {
            refreshCombos.run();
            refreshTable.run();
            updateButtonsState.run();
            logArea.appendText("✓ Lists refreshed.\n");
            UiUtils.showInfo("Lists refreshed", "Doctor and patient lists were refreshed.");
        });

        generateButton.setOnAction(e -> {
            Doctor d = doctorCombo.getValue();
            LocalDate date = datePicker.getValue();
            if (d == null || date == null) {
                UiUtils.showError("Missing selection", "Please select doctor and date.");
                logArea.appendText("Error: doctor/date not selected for slot generation.\n");
                return;
            }

            String timeSlot = d.getTimeSlot();
            if (timeSlot == null || !timeSlot.matches("\\d{2}:\\d{2}-\\d{2}:\\d{2}")) {
                UiUtils.showError(
                        "Invalid time slot",
                        "Doctor " + d.getName() + " has an invalid time slot: " + timeSlot +
                        "\nExpected format: HH:MM-HH:MM (e.g. 09:00-17:00)."
                );
                logArea.appendText("Error: invalid time slot for doctor ID " + d.getId() + ".\n");
                return;
            }

            String dateStr = date.toString();
            hospitalSystem.generateAvailableAppointments(d, dateStr);
            refreshTable.run();
            updateButtonsState.run();

            logArea.appendText("✓ Slots generated for " + d.getName()
                               + " on " + dateStr + ".\n");
            UiUtils.showInfo("Slots generated",
                    "Available slots generated for Dr. " + d.getName()
                    + " on " + dateStr + ".");
        });

        bookButton.setOnAction(e -> {
            Appointment selected = table.getSelectionModel().getSelectedItem();
            Patient p = patientCombo.getValue();

            if (selected == null) {
                UiUtils.showError("No appointment selected", "Select an appointment slot first.");
                logArea.appendText("Error: no appointment selected for booking.\n");
                return;
            }
            if (p == null) {
                UiUtils.showError("No patient selected", "Select a patient first.");
                logArea.appendText("Error: no patient selected for booking.\n");
                return;
            }

            selected.setPatient(p);
            selected.setStatus(true);
            refreshTable.run();
            updateButtonsState.run();

            logArea.appendText("✓ Booked: " + p.getName()
                               + " with " + selected.getDoctor().getName()
                               + " at " + selected.getDate() + " " + selected.getTime() + ".\n");
            UiUtils.showInfo("Appointment booked",
                    "Appointment booked for " + p.getName()
                    + " with Dr. " + selected.getDoctor().getName()
                    + " at " + selected.getDate() + " " + selected.getTime() + ".");
        });

        cancelButton.setOnAction(e -> {
            Appointment selected = table.getSelectionModel().getSelectedItem();
            if (selected == null) {
                UiUtils.showError("No selection", "Select an appointment to cancel.");
                logArea.appendText("Error: no appointment selected for cancel.\n");
                return;
            }

            boolean ok = UiUtils.confirm(
                    "Confirm cancel",
                    "Are you sure you want to cancel appointment ID "
                    + selected.getId() + " at " + selected.getTime() + "?"
            );
            if (!ok) {
                return;
            }

            selected.setPatient(null);
            selected.setStatus(false);
            refreshTable.run();
            updateButtonsState.run();

            logArea.appendText("✓ Appointment ID " + selected.getId()
                               + " marked as available again.\n");
            UiUtils.showInfo("Appointment cancelled",
                    "Appointment ID " + selected.getId() + " is now free.");
        });

        showBookedButton.setOnAction(e -> {
            Doctor d = doctorCombo.getValue();
            LocalDate date = datePicker.getValue();
            if (d == null || date == null) {
                UiUtils.showError("Missing selection", "Select doctor and date to filter.");
                logArea.appendText("Error: Doctor/date not selected for 'Show Booked'.\n");
                return;
            }
            String dateStr = date.toString();

            List<Appointment> filtered = hospitalSystem.getAppointments()
                    .stream()
                    .filter(a -> a.getDoctor().getId() == d.getId()
                              && a.getDate().equals(dateStr)
                              && Boolean.TRUE.equals(a.getStatus()))
                    .collect(Collectors.toList());

            displayedAppointments.setAll(filtered);
            updateHeaderAndSummary.run();
            updateButtonsState.run();
        });

        showAvailableButton.setOnAction(e -> {
            Doctor d = doctorCombo.getValue();
            LocalDate date = datePicker.getValue();
            if (d == null || date == null) {
                UiUtils.showError("Missing selection", "Select doctor and date to filter.");
                logArea.appendText("Error: Doctor/date not selected for 'Show Available'.\n");
                return;
            }
            String dateStr = date.toString();

            List<Appointment> filtered = hospitalSystem.getAppointments()
                    .stream()
                    .filter(a -> a.getDoctor().getId() == d.getId()
                              && a.getDate().equals(dateStr)
                              && Boolean.FALSE.equals(a.getStatus()))
                    .collect(Collectors.toList());

            displayedAppointments.setAll(filtered);
            updateHeaderAndSummary.run();
            updateButtonsState.run();
        });

        // initial population
        refreshCombos.run();
        refreshTable.run();
        updateButtonsState.run();

        return root;
    }
}
