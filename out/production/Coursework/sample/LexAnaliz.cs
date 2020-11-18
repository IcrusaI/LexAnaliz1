using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Globalization;
using System.Diagnostics;
using kr;
using System.Collections;
using System.Configuration;
using System.Runtime.Remoting.Messaging;

namespace kr
{
    public class Variable
    {
        public string name;
        public string type;

        public Variable(string nameParam, string typeParam) {
            name = nameParam;
            type = typeParam;
        }
    }

    public class NumberBinary
    {
        public string original;

        public NumberBinary(string originalParam) {
            original = originalParam;
        }

        public string binary
        {
            get {
                string buf = original.Remove(original.Length - 1, 1);
                long temp = 0;
                if (new Regex("^[0-9]+$").IsMatch(original)) { // десятичное число
                    temp = Convert.ToInt32(original);
                } else if (new Regex("^([0-9]+)([eE][-+]?[0-9]+)?$").IsMatch(original)) { // Экспоненциальная форма
                    decimal.TryParse(original, NumberStyles.Float, CultureInfo.InvariantCulture, out decimal buf0);

                    temp = BitConverter.DoubleToInt64Bits(Convert.ToDouble(buf0));
                } else {
                    switch (original[original.Length - 1]) {
                        case 'b': // Двоичное
                        case 'd': // Десятичное
                            temp = Convert.ToInt32(buf);
                            break;
                        case 'o': // Восьмеричное
                            temp = Convert.ToInt64(buf, 8);
                            break;
                        case 'h': // Шеснадцатиричное
                            temp = Convert.ToInt64(buf, 16);
                            break;
                    }
                }

                return Convert.ToString(temp, 2);
            }
        }
    }

    public class Rules
    {
        public string[] code;
        private int index;
        
        public List<Variable> variables = new List<Variable>();
        public List<NumberBinary>  numbers = new List<NumberBinary>();
        public List<string> resultAnalysis = new List<string>();
        
        private protected List<string> numberOperations = new List<string>() { "mult", "div", "min", "plus" };
        private protected List<string> supportTypesVariable = new List<string>() { "int", "float", "bool" };
        private protected string[] TableKeyword = { "begin", "end", "var", "int", "float", "bool", "if", "else", "for", "to", "step", "next", "while", "readln", "writeln", "true", "false" };
        private protected string[] TableSeparators = { "ne", "eq", "lt", "le", "gt", "ge", "plus", "min", "or", "mult", "div", "and", "~", "(", ")", ";", ",", ":", "=", "%" };

        public Rules(string codeArray) {
            // Переводим код в массив строк
            string[] stringSeparators = new string[] { "\n" };
            string[] lines = codeArray.Split(stringSeparators, StringSplitOptions.None);

            code = lines;
            // записывать все сепаратары, типы, ключевые слова в черный список
        }

        public void Scanner() {
            variables = new List<Variable>();
            numbers = new List<NumberBinary>();
            resultAnalysis = new List<string>();
            index = 0;

            IsProgram();
        }

        private Boolean IsProgram() {
            if (!code[index].TrimEnd().Equals("begin")) { // Проверяем начало программы
                throw new Exception("Программа должна начинаться с begin");
            } else {
                addToResultAnalysis(0, "begin");
                index++;
            }

            InitializationOperators(); // проверяем есть ли переменные
            
            if (code[index].TrimEnd().Equals("\tbegin")) { // проверяем что тело программы начинается с begin
                addToResultAnalysis(0, "begin");
                index++;
            } else { // иначе выдаем ошибку
                  throw new Exception("Тело программа должна начинаться с begin");
            }

            IsBody(2);

             if (code[index].TrimEnd().Equals("\tend") & code[index + 1].TrimEnd().Equals("end")) { // выход из тела
                addToResultAnalysis(0, "end");
                addToResultAnalysis(0, "end");
                    return true;
             } else {
                     throw new Exception("Программа должна заканчиваться end");
             } 
        }

        private Boolean IsBody(int indent) {
            while(index < code.Length) { // проверяем код
                SkipLineTranslation();

                string codeString = code[index]; // проверяем табуляцию тела

                if (new Regex("^" + new String('\t', indent) + "(?!\t).+$").IsMatch(codeString)) {
                    codeString = codeString.Substring(indent).TrimEnd();
                } else if (new Regex("^" + new String('\t', indent - 1) + "(?!\t).+$").IsMatch(codeString)) { // выход из тела
                    break;
                } else {
                     throw new Exception("Неизвестный исход, тело должно быть на " + indent + " уровень выше");
                }
                codeString = RemoveDescription(codeString).TrimEnd();
                // код проверка

                if (!(
                    IsSetVariable(codeString) | // переменные
                    IsIf(indent) | // if
                    IsFor(indent) | // for
                    IsWhile(indent) | // while
                    IsWriteLn(codeString) | // writeln
                    IsReadLn(codeString) // readln
                    )) {
                    throw new Exception("Ошибка в чтении");
                }

                index++;
            }

            return true;
        }

        private Boolean InitializationOperators() { // инициализация блока var
            if (code[index].TrimEnd().Equals("\tvar")) { // проверяем задаются ли переменные
                addToResultAnalysis(0, "var");
                index++;
                Regex regex = new Regex("(^([A-Za-z0-9]{1,},?){1,}:[a-z]{1,};$)");

                while (index < code.Length) {
                     SkipLineTranslation();

                    if (code[index].TrimEnd().Equals("\tbegin")) {
                       break;
                    }
                    if (!regex.IsMatch(code[index].Replace("\t", "").TrimEnd())) {
                        throw new Exception("Неправильно синтаксис переменных");
                    }

                    string[] variablesRaw = code[index].Trim().Replace(";", "").Replace(" ", "").Split(',', ':');
                    string type = variablesRaw[variablesRaw.Length - 1];

                    if (supportTypesVariable.Find(x => x.Contains(type)) == null) { // проверяем поддерживаемый тип
                        throw new Exception("Тип " + type + " не поддерживается");
                    }

                    for (int i = 0; i < variablesRaw.Length - 1; i++) { // проверяем созданную переменную
                        if (checkExistVariable(variablesRaw[i])) {
                            throw new Exception("Переменная " + variablesRaw[i] + " уже существует");
                        }
                        variables.Add(new Variable(variablesRaw[i], type));

                        addToResultAnalysis(2, variablesRaw[i]);
                        if (variablesRaw.Length - 1 - i > 1) {
                            addToResultAnalysis(1, ",");
                        }
                    }

                     addToResultAnalysis(1, ":");
                    addToResultAnalysis(0, type);
                    addToResultAnalysis(1, ";");

                    index++;
                }
            }
           return true;
        }

        private Boolean IsWriteLn(string data) {
            Boolean result = new Regex("^writeln (\\(?(\"[^\"]+\"|[A-Za-z]+)\\)?,? ?)+;$").IsMatch(data);
            if (result) {
                addToResultAnalysis(0, "writeln");
            }
            return result;
        }
        
        private Boolean IsReadLn(string data) {
            Boolean result = new Regex("^readln ([a-zA-Z0-9]+,? ?)+;$").IsMatch(data);
            if (result) {
                addToResultAnalysis(0, "readln");
            }
            return result;
        }

        private Boolean IsWhile(int indent) {
            if (new Regex(@"^while\s?\((.+)\)$").IsMatch(code[index].Trim())) {
                addToResultAnalysis(0, "while");
                addToResultAnalysis(1, "(");
                addToResultAnalysis(1, ")");
                index++;
                IsBody(indent + 1);

                return true;
            } else {
                return false;
            }
        }

        private Boolean IsFor(int indent) {
            Regex regex = new Regex("^for (.+) to (.+) step (.+)$");
           
            if (regex.IsMatch(code[index].Trim())) {
                addToResultAnalysis(0, "for");
                addToResultAnalysis(0, "to");
                addToResultAnalysis(0, "step");

                Match match = regex.Match(code[index].Trim());
           
                string variable = match.Groups[1].ToString();
                string end = match.Groups[2].ToString();
                string step = match.Groups[3].ToString();
            
                IsSetVariable(variable);
                if (!(CheckNumber(end) != "" | CheckNumber(step) != "")) {
                    throw new Exception("Не является числом");
                }
                AddNumberToList(new NumberBinary(end));
                AddNumberToList(new NumberBinary(step));

                index++;
                IsBody(indent + 1);

                if (!code[index].Substring(indent).TrimEnd().Equals("next")) {
                    throw new Exception("Цикл for должен завершаться next");
                }
                addToResultAnalysis(0, "next");

                return true;
            } else {
                return false;
            }
        }

        private Boolean IsIf(int indent) {
            if (new Regex(@"^if\s?\((.+)\)$").IsMatch(code[index].Trim())) {
                addToResultAnalysis(0, "if");
                addToResultAnalysis(1, "(");
                addToResultAnalysis(1, ")");
                index++;
                IsBody(indent + 1);
                
                if (code[index].Substring(indent).TrimEnd().Equals("else")) {
                    addToResultAnalysis(0, "else");
                    index++;
                    IsBody(indent + 1);
                }
                return true;
            } else {
                return false;
            }
        }

        private Boolean IsSetVariable(string data) {
            if (new Regex("^[A-Za-z0-9]{1,}[[:space:]]?:=.+;$").IsMatch(data)) {
                string variableRaw = data.Split(new [] {":=" }, StringSplitOptions.None)[0].Trim();
                string valueRaw = data.Split(new [] {":=" }, StringSplitOptions.None)[1].Trim();
                
                if (!checkExistVariable(variableRaw)) {
                    throw new Exception("Переменной " + variableRaw + " не существует");
                }

                 Variable variable = variables.Find(x => x.name.Contains(variableRaw) );

                addToResultAnalysis(2, variable.name);
                addToResultAnalysis(1, ":");
                addToResultAnalysis(1, "=");

                // проверяем вставляемое значение
                switch (variable.type) {
                    case "int":
                    case "float":
                        CheckOperationWithNumber(valueRaw.Substring(0, valueRaw.Length - 1), variable.type);
                        break;
                    case "boolean":
                        CheckBoolean(valueRaw.Substring(0, valueRaw.Length - 1));
                        break;
                }

                addToResultAnalysis(1, ";");

                return true;
            } else {
                return false;
            }
        }

        private Boolean CheckOperationWithNumber(string data, string type) {
            string[] operationWithNumber = data.Split(new [] {" "}, StringSplitOptions.None);
            if (CheckNumber(data) != "") {
                if (type == "int") {
                    CheckOperationWithIntType(data);
                }

                AddNumberToList(new NumberBinary(data));
                return true;
            } else if (operationWithNumber.Length == 3) {
                if (numberOperations.Find(x => x == operationWithNumber[1]) == null) {
                    throw new Exception(operationWithNumber[1] + " математического оператора не существует");
                }

                if (CheckNumber(operationWithNumber[0]) != "") {
                    AddNumberToList(new NumberBinary(operationWithNumber[0]));
                } else if (checkExistVariable(operationWithNumber[0])) {
                    addToResultAnalysis(2, operationWithNumber[0]);
                } else {
                     throw new Exception("Это не число, либо переменная");
                }

                if (type == "int") {
                    CheckOperationWithIntType(null, operationWithNumber[1]);
                }
                
                addToResultAnalysis(1, operationWithNumber[1]);

                if (CheckNumber(operationWithNumber[2]) != "") {
                    AddNumberToList(new NumberBinary(operationWithNumber[2]));
                } else if (checkExistVariable(operationWithNumber[2])) {
                    addToResultAnalysis(2, operationWithNumber[2]);
                } else {
                     throw new Exception("Это не число, либо переменная");
                }


            } else {
                throw new Exception("Это не операция с числом");
            }

            return false;
        }

        private Boolean CheckOperationWithIntType(string data=null, string operation=null) {
            if (data != null && CheckNumber(data) == "e") {
                throw new Exception("Нельзя в int присвоить экспоненциальную форму");
            }

            if (operation != null && operation == "div") {
                throw new Exception("Нельзя в int делить");
            }

            return true;
        }

        private Boolean CheckBoolean(string data) {
            if (!(data == "true" | data == "false")) {
                throw new Exception("В переменную записывается не булевское значение");
            }
            addToResultAnalysis(0, data);

            return true;
        }

        private string CheckNumber(string data) { // проверяет число ли это

            Debug.WriteLine(data == null);
            if (new Regex("^([0-9]+)([eE][-+]?[0-9]+)?$").IsMatch(data)) { // Экспоненциальная форма
                return "e";
            }

            if (new Regex("^[0-9]+$").IsMatch(data)) { // десятичное число
                return "d";
            }

            switch (data.Substring(data.Length - 1, 1)) {
                case "b": // двоичное
                    if (new Regex("^[01]+b$").IsMatch(data)) {
                        return "b";
                    }
                break;
                case "o": // восьмиричное
                    if (new Regex("^[0-7]+o$").IsMatch(data)) {
                        return "o";
                    }
                break;
                case "d": // десятичный
                    if (new Regex("^[0-9]+d$").IsMatch(data)) {
                        return "d";
                    }
                break;
                case "h": // %шестнадцатирич
                    if (new Regex("^[0-9a-fA-F]+h$").IsMatch(data)) {
                        return "h";
                    }
                break;
            }

                return "";
        }

        private string RemoveDescription(string data) {
            string newData = new Regex("%.+").Replace(data, "");

            if (data != newData) {
                addToResultAnalysis(1, "%");
                addToResultAnalysis(1, "%");
            }

            return newData;
        } // валидация комментария

        private Boolean SkipLineTranslation()
        {
            if (code[index].Trim().Equals(""))
            {
                index++;
                return true;
            }
            else
            {
                return false;
            }
        } // пропуск пустых строк

        private Boolean checkExistVariable(string searchName) {
            return variables.Find(x => x.name.Contains(searchName) ) != null;
        }

        private Boolean AddNumberToList(NumberBinary data) {
            if (numbers.Find(e => e.original == data.original) == null) { 
                numbers.Add(data);
            }

            addToResultAnalysis(3, data.original);
            
            return true;
        }

        private Boolean addToResultAnalysis(int idTable, string element) {
            int idElement = -1;

            switch (idTable) {
                case 0:
                    idElement = Array.IndexOf(TableKeyword, element);
                    break;
                case 1:
                    idElement = Array.IndexOf(TableSeparators, element);
                    break;
                case 2:
                    idElement = variables.FindIndex(e => e.name == element);
                    break;
                case 3:
                    idElement = numbers.FindIndex(e => e.original == element);
                    break;
                default:
                    return false;
            }


            if (idElement == -1) {
                return false;
            }

            resultAnalysis.Add("(" + idTable.ToString() + ", " + idElement.ToString() + ")");

            return true;
        }
    }

    public partial class LexAnaliz : Form
    {
        string[] TableKeyword = { "begin", "end", "var", "int", "float", "boolean", "if", "else", "for", "to", "step", "next", "while", "readln", "writeln", "true", "false" };
        string[] TableSeparators = { "ne", "eq", "lt", "le", "gt", "ge", "plus", "min", "or", "mult", "div", "and", "~", "(", ")", ";", ",", ":", "=", "%" };

        List<string> TableDigit = new List<string>();
        List<string> TableIdentifier = new List<string>();

        public LexAnaliz()
        {
            InitializeComponent();
            for (int i = 0; i < TableKeyword.Length; i++)
            {
                TextTableKeyword.AppendText(i + ". " + TableKeyword[i] + "\n");
            }
            for (int i = 0; i < TableSeparators.Length; i++)
            {
                TextTableSeparators.AppendText(i + ". " + TableSeparators[i] + "\n");
            }
        }

        private void Scaner(string code)
        {

            Rules rules = new Rules(code);
       
            try {
                rules.Scanner();

                 for (int k = 0; k < rules.variables.Count; k++)
            {
                TextTableIdentifier.AppendText(k + ". " + rules.variables[k].name + ": " + rules.variables[k].type + "\n");
            }

            for (int k = 0; k < rules.numbers.Count; k++)
            {
                TextTableDigit.AppendText(k + ". " + " " + rules.numbers[k].original + " = " + rules.numbers[k].binary + "\n");
            }

            for (int k = 0; k < rules.resultAnalysis.Count; k++)
            {
                    TextTokens.AppendText(rules.resultAnalysis[k]);
                    if (rules.resultAnalysis.Count - k > 1) {
                        TextTokens.AppendText(", ");
                    } else {
                        TextTokens.AppendText(".");
                    }
            }
                TextError.AppendText("Лексический анализ успешно завершен.\n");
            } catch (Exception err) {
                TextError.AppendText(err.Message + "\n");
            } 
        }

        private void загрузитьФайлToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (OpenFile.ShowDialog() == DialogResult.Cancel)
                return;
            // получаем выбранный файл
            string filename = OpenFile.FileName;
            // читаем файл в строку
            string fileText = System.IO.File.ReadAllText(filename);
            TextProgram.Text = fileText;
            MessageBox.Show("Файл открыт");
        }

        private void сохранитьФайлToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (SaveFile.ShowDialog() == DialogResult.Cancel)
                return;
            // получаем выбранный файл
            string filename = SaveFile.FileName;
            // сохраняем текст в файл
            System.IO.File.WriteAllText(filename, TextTokens.Text);
            MessageBox.Show("Файл сохранен");
        }

        private void произвестилексическийанализToolStripMenuItem_Click(object sender, EventArgs e)
        {
            TextError.Clear();
            TextTableDigit.Clear();
            TextTableIdentifier.Clear();
            TextTokens.Clear();

            Scaner(TextProgram.Text);
        }

        private void выходToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}

