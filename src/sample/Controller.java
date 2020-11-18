package sample;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextArea;

import java.util.ArrayList;
import java.util.List;

public class Controller {
    @FXML
    private ListView<String> keywords;

    @FXML
    private ListView<String> separators;

    @FXML
    private ListView<String> variables;

    @FXML
    private ListView<String> numbers;

    @FXML
    private TextArea output;

    @FXML
    private Button checkButton;

    @FXML
    private TextArea code;

    public void initialize() {
        setKeywords();
        setSeparators();
    }

    public void onClickCheck() throws Exception {
        Analizator analizator = new Analizator(code.getText());

        try {
            analizator.scanner();

            setResult(analizator.result);
        } catch (Exception err) {
            setOutput(err.getMessage());
        } finally {
            setVariables(analizator.variables);
            setNumbers(analizator.numbers);
        }
    }

    private void setKeywords() {
        ObservableList data = FXCollections.observableArrayList();

        for (int i = 0; i < Analizator.keywords.size(); i++) {
            data.add((i + 1) + ". " + Analizator.keywords.get(i));
        }
        keywords.setItems(data);
    }

    private void setSeparators() {
        ObservableList data = FXCollections.observableArrayList();

        for (int i = 0; i < Analizator.separators.size(); i++) {
            data.add((i + 1) + ". " + Analizator.separators.get(i));
        }
        separators.setItems(data);
    }

    private void setVariables(List<Variable> variableList) {
        ObservableList data = FXCollections.observableArrayList();

        for (int i = 0; i < variableList.size(); i++) {
            data.add((i + 1) + ". " + variableList.get(i).name + ": " + variableList.get(i).type);
        }
        variables.setItems(data);
    }

    private void setNumbers(List<NumberBinary> numberBinaries) {
        ObservableList data = FXCollections.observableArrayList();

        for (int i = 0; i < numberBinaries.size(); i++) {
            data.add((i + 1) + ". " + numberBinaries.get(i).original + ": " + numberBinaries.get(i).getBinary());
        }
        numbers.setItems(data);
    }

    private void setResult(List<String> result) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < result.size(); i++) {
            data.append(result.get(i));
            if (result.size() - i > 1) {
                data.append(", ");
            } else {
                data.append(".");
            }
        }

        setOutput(data.toString());
    }

    private void setOutput(String data) {
        output.setText(data);
    }
}
