package sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Variable {
    public String name;
    public String type;

    public Variable(String nameParam, String typeParam) {
        name = nameParam;
        type = typeParam;
    }
}

class NumberBinary {
    public String original;

    public NumberBinary(String originalParam) {
        original = originalParam;
    }

    public String getBinary() {
        String buf = original.substring(0, original.length() - 1);
        int temp = 0;

        if (original.matches("^[0-9]+$")) { // десятичное число
            temp = Integer.parseInt(original);
        } else if (original.matches("^([0-9]+)([eE][-+]?[0-9]+)$")) { // Экспоненциальная форма
            temp = Double.valueOf(original).intValue();
        } else {
            switch (original.substring(original.length() - 1)) {
                case "b": // Двоичное
                case "d": // Десятичное
                    temp = Integer.parseInt(buf);
                    break;
                case "o": // Восьмеричное
                    temp = Integer.parseInt(buf, 8);
                    break;
                case "h": // Шеснадцатиричное
                    temp = Integer.parseInt(buf, 16);
                    break;
            }
        }

        return Integer.toString(temp, 2);
    }
}

public class Analizator {
    public static List<String> variableTypes = Arrays.asList("int", "float", "bool");
    public static List<String> keywords = Arrays.asList("begin", "dim", "end", "int", "float", "bool", "let", "if", "then", "else", "for", "do", "while", "loop", "input", "output", "true", "false");
    public static List<String> separators = Arrays.asList("!=", "==", "<", "<=", ">", ">=", "+", "-", "||", "*", "/", "&&", "!", "(", ")", ".", ";", ",", "=", "{", "}", "_", " ", "%");

    private static final String variableRegex = "(?![0-9])[0-9A-Za-z]+";
    private static final List<String> mathematicalOperators = Arrays.asList("+", "-", "*", "/");

    List<Variable> variables = new ArrayList<Variable>();
    List<NumberBinary> numbers = new ArrayList<NumberBinary>();
    List<String> result = new ArrayList<String>();

    public List<String> code;
    private int index = 0;

    private String getCurrentString() {
        // Удаляем лишние пробелы в конце строки
        return code.get(index).replaceFirst("[\t\s]+$", "");
    }

    public Analizator(String data) {
        // Разбиваем код построчно и записываем его в List code
        code = Arrays.asList(data.split("\n"));
    }

    public void scanner() throws Exception {
        index = 0;

        isProgram();
    }

    private Boolean isProgram() throws Exception {
        skipLineTranslation();

        // Инициализация программы
        if (!getCurrentString().equals("begin")) {
            throw new Exception("Программа должна начинаться с begin");
        }
        addToResult(1, "begin");
        index++;

        initializationDim();

        parseBody(1);

        if (!parseString(getCurrentString(), 0).equals("end")) {
            throw new Exception("Программа должна закачиваться end.");
        } else {
            addToResult(1, "end");
        }

        return true;
    }

    // Инициализация блока dim
    private void initializationDim() throws Exception {
        if (deleteLeftIndent(getCurrentString(), 1).equals("dim")) {
            addToResult(1, "dim");
            index++;

            while (index < code.size()) {
                skipLineTranslation();

                // Выход из цикла
                if (getCurrentString().matches("^\t(?!\t).+$")) {
                    break;
                }

                // Проверка отступа
                if (!getCurrentString().matches("^\t{2}.+;$")) {
                    throw new Exception("Неправильный синтаксис переменных");
                }

                String data = deleteLeftIndent(getCurrentString(), 2);
                data = data.substring(0, data.length() - 1);

                parseVariablesDim(data);
                addToResult(2, ";");
                index++;
            }
        }
    }

    private void parseBody(Integer indent) throws Exception {
        while (index < code.size()) {
            skipLineTranslation();

            String data = parseString(getCurrentString(), indent);

            if (data == null) {
                break;
            }


            if (!(isSetVariable(data) | isIf(data, indent) | isFor(data, indent) | isDoWhile(data, indent) | isWriteLn(data) | isReadLn(data))) {
                throw new Exception("Ошибка в чтении");
            }

            index++;
        }
    }

    private String parseString(String data, int indent) throws Exception {
        if (data.matches("^\t{" + indent + "}(?!\t).+$")) { // проверяем отступ слева
            data = data.substring(indent);
        } else if (data.matches("^\t{" + (indent - 1) + "}(?!\t).+$")) { // выход из тела
            return null;
        } else {
            throw new Exception("Неизвестный код, тело должно быть на " + indent + " уровне");
        }

        return deleteDescription(data).trim();
    }

    private void parseVariablesDim(String data) throws Exception {
        String[] rawVariables = data.split("\s?,\s?|\s");

        if (rawVariables.length < 2) {
            throw new Exception("Неправильный синтаксис переменных.");
        }

        String type = rawVariables[0];

        if (isSupportTypeVariable(type)) {
            throw new Exception("Тип: " + type + " не поддерживается.");
        }

        addToResult(1, type);
        addToResult(2, " ");


        for (int i = 1; i < rawVariables.length; i++) {
            String name = rawVariables[i];

            if (!name.matches("^" + variableRegex + "$")) {
                throw new Exception("Не поддерживаемое название переменной.");
            }

            if (checkExistVariable(name)) {
                throw new Exception("Переменная " + name + " уже существует.");
            }

            variables.add(new Variable(name, type));
            addToResult(3, name);
            if (rawVariables.length - 1 - i > 1) {
                addToResult(2, ",");
            }
        }
    }

    private Boolean checkExistVariable(String name) {
        for (Variable variable : variables) {
            if (variable.name.equals(name)) {
                return true;
            }
        }

        return false;
    }

    private Boolean isSupportTypeVariable(String type) {
        return variableTypes.equals(type);
    }

    private Boolean isSetVariable(String data) throws Exception {
        String regex = "^(let )?(" + variableRegex + ")\s?=\s?(.+);$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(data);

        if (matcher.matches()) {
            String name = matcher.group(2);
            String value = matcher.group(3);
            boolean isLet = matcher.group(1) != null;

            if (!checkExistVariable(name)) {
                throw new Exception("Переменной " + name + " не существует.");
            }

            Variable variable = null;
            for (Variable variableData : variables) {
                if (variableData.name.equals(name)) {
                    variable = variableData;
                }
            }

            if (isLet) {
                addToResult(1, "let");
            }
            assert variable != null;
            addToResult(3, variable.name);
            addToResult(2, "=");

            // Проверяем вставляемое значение
            switch (variable.type) {
                case "int", "float" -> parseOperationWithNumber(value, variable.type);
                case "bool" -> parseOperationWithBoolean(value, variable.type);
            }

            addToResult(2, ";");
            return true;
        } else {
            return false;
        }
    }

    private Boolean isWriteLn(String data) {
        Boolean result = data.matches("^output (\\(?(\"[^\"]+\"|[A-Za-z]+)\\)?,? ?)+;$");
        if (result) {
            addToResult(1, "output");
        }
        return result;
    }

    private Boolean isReadLn(String data) {
        Boolean result = data.matches("^input \\(([a-zA-z0-9]+,?\\s?)+\\);$");
        if (result) {
            addToResult(1, "input");
        }
        return result;
    }

    private Boolean parseOperationWithNumber(String data, String type) throws Exception {
        String[] raw = data.split("\s", 3);

        if (type.equals("int")) {
            checkOperationWithIntType(data, type);
        }

        if (!getTypeNumber(data).equals("")) {
            addToResult(4, data);

            return true;
        } else if (raw.length == 3) {
            checkMathematicalExpression(raw[0], raw[1], raw[2], mathematicalOperators);

            return true;
        } else {
            throw new Exception("Это не операция с числом");
        }
    }

    private void checkNumberOrVariable(String data) throws Exception {
        if (!getTypeNumber(data).equals("")) {
            addToResult(4, data);
        } else if (checkExistVariable(data)) {
            addToResult(3, data);
        } else if (parseOperationWithNumber(data, "int")) {

        } else {
            throw new Exception(data + " не число, либо переменная");
        }
    }

    private void checkMathematicalExpression(String e1, String operator, String e2, List<String> operators) throws Exception {
        if (operators.equals(operator)) {
            throw new Exception(operator + " математического оператора не существует");
        }

        checkNumberOrVariable(e1);

        addToResult(2, operator);

        checkNumberOrVariable(e2);
    }

    private void checkOperationWithIntType(String data, String operation) throws Exception {
        if (data != null && getTypeNumber(data).equals("e")) {
            throw new Exception("Нельзя присвоить экспоненциальную форму в int.");
        }

        if (operation.equals("div")) {
            throw new Exception("Нельзя делить в int.");
        }
    }

    private void parseOperationWithBoolean(String data, String type) throws Exception {
        if (!(data.equals("true") | data.equals("false"))) {
            throw new Exception("В переменную можно записывать только bool");
        }

        addToResult(1, data);
    }

    private String getTypeNumber(String data) {
        if (data.matches("^([0-9]+)([eE][-+]?[0-9]+)$")) { // Экспоненциальная форма
            return "e";
        }

        if (data.matches("^[+-]?[0-9]+$")) { // десятичное число
            return "d";
        }

        var substring = data.substring(data.length() - 1);
        switch (substring) {
            case "b": // двоичное
                if (data.matches("^[01]+b$")) {
                    return "b";
                }
                break;
            case "o": // восьмиричное
                if (data.matches("^[0-7]+o$")) {
                    return "o";
                }
                break;
            case "d": // десятичный
                if (data.matches("^[0-9]+d$")) {
                    return "d";
                }
                break;
            case "h": // шестнадцатирич
                if (data.matches("^[0-9a-fA-F]+h$")) {
                    return "h";
                }
                break;
        }

        return "";
    }

    private Boolean isIf(String data, int indent) throws Exception {
        String regex = "^if\s(.+)\sthen$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(data);

        if (matcher.matches()) {
            addToResult(1, "if");
            addToResult(2, " ");

            String[] head = matcher.group(1).split("\s", 3);

            checkHeadIf(head);
            index++;

            addToResult(2, " ");
            addToResult(1, "then");

            parseBody(indent + 1);

            if (parseString(getCurrentString(), indent).equals("else")) {
                addToResult(1, "else");
                index++;
                parseBody(indent + 1);
                if (!parseString(getCurrentString(), indent).equals("end_else")) {
                    throw new Exception("If должен заканчиваться end_else");
                } else {
                    addToResult(1, "end_else");
                    index++;
                }
            }

            index--;

            return true;
        } else {
            return false;
        }
    }

    private Boolean isFor(String data, int indent) throws Exception {
        String regex = "^for\s?\\((.+)\\)$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(data);

        if (matcher.matches()) {
            addToResult(1, "for");
            addToResult(2, "(");

            String[] head = matcher.group(1).split("\s?;\s?");

            checkHeadFor(head);

            addToResult(2, ")");
            index++;

            parseBody(indent + 1);
            index--;

            return true;
        } else {
            return false;
        }
    }

    private void checkHeadFor(String[] data) throws Exception {
        isSetVariable(data[0]);

        String[] raw = data[1].split("\s");
        checkHeadIf(raw);

        isSetVariable(data[2]);
    }

    private Boolean isDoWhile(String data, int indent) throws Exception {
        String regex = "^do\swhile\s(.+)$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(data);

        if (matcher.matches()) {
            addToResult(1, "do");
            addToResult(1, "while");

            String let = matcher.group(1);

            isSetVariable(let);
            index++;

            parseBody(indent + 1);

            if (!parseString(getCurrentString(), indent).equals("loop")) {
                throw new Exception("While должен заканчиваться loop");
            } else {
                addToResult(1, "loop");
                index++;
            }

            index--;

            return true;
        } else {
            return false;
        }
    }

    private void checkHeadIf(String[] data) throws Exception {
        List<String> mathematicalOperators = Arrays.asList("!=", "==", "<", "<=", ">", ">=", "||", "&&");

        checkMathematicalExpression(data[0], data[1], data[2], mathematicalOperators);

    }

    private Boolean skipLineTranslation() {
        if (code.get(index).trim().equals("")) {
            index++;
            return skipLineTranslation();
        }

        return true;
    }

    private Boolean addToResult(Integer id, String element) {
        int idElement = -1;

        switch (id) {
            case 1: // Ключевые слова
                idElement = keywords.indexOf(element);
                break;
            case 2: // Разделители
                idElement = separators.indexOf(element);
                break;
            case 3: // Идентификаторы
                for (int i = 0; i < variables.size(); i++) {
                    if (variables.get(i).name.equals(element)) {
                        idElement = i;
                        break;
                    }
                }
                break;
            case 4: // Числа
                int index = -1;
                for (int i = 0; i < numbers.size(); i++) {
                    if (numbers.get(i).original.equals(element)) {
                        index = i;
                    }
                }
                if (index == -1) {
                    numbers.add(new NumberBinary(element));
                    index = numbers.size() - 1;
                }

                idElement = index;
                break;
            default:
                return false;
        }


        if (idElement == -1) {
            return false;
        }

        idElement++;
        result.add("(" + id.toString() + ", " + Integer.toString(idElement) + ")");

        return true;
    }

    private String deleteDescription(String data) {
        String newData = data.replaceFirst("%(.+)%$", "").trim();

        if (!newData.equals(data)) {
            addToResult(2, "%");
            addToResult(2, "%");
        }
        return newData;
    }

    private String deleteLeftIndent(String data, Integer indent) {
        return data.replaceFirst("^\t{" + indent + "}", "");
    }

}
