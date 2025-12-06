package hospitalAppointmentSystem;

import javafx.animation.ScaleTransition;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.stage.Stage;
import javafx.util.Duration;

public class MainApp extends Application {

    private HospitalSystem hospitalSystem;

    @Override
    public void start(Stage primaryStage) {
        hospitalSystem = new HospitalSystem();

        TabPane tabPane = new TabPane();

        Tab doctorTab = new Tab("Doctors");
        doctorTab.setClosable(false);
        doctorTab.setContent(DoctorView.create(hospitalSystem));

        Tab patientTab = new Tab("Patients");
        patientTab.setClosable(false);
        patientTab.setContent(PatientView.create(hospitalSystem));

        Tab appointmentTab = new Tab("Appointments");
        appointmentTab.setClosable(false);
        appointmentTab.setContent(AppointmentView.create(hospitalSystem));

        tabPane.getTabs().addAll(doctorTab, patientTab, appointmentTab);

        // small scale animation when switching tabs
        tabPane.getSelectionModel().selectedItemProperty().addListener((obs, oldTab, newTab) -> {
            if (newTab != null && newTab.getContent() != null) {
                ScaleTransition st = new ScaleTransition(Duration.millis(180), newTab.getContent());
                st.setFromX(0.97);
                st.setFromY(0.97);
                st.setToX(1.0);
                st.setToY(1.0);
                st.play();
            }
        });

        Scene scene = new Scene(tabPane, 900, 650);

        // attach CSS (next section)
        scene.getStylesheets().add(
                MainApp.class.getResource("style.css").toExternalForm()
        );

        primaryStage.setScene(scene);
        primaryStage.setTitle("Hospital Appointment System");
        primaryStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
