package parseTestForInterpreter;

import java.io.*;

public class ParseTests {

	public String readText (String filename){
		StringBuilder sb = new StringBuilder();
		try{
			BufferedReader br = new BufferedReader(new FileReader(new File (filename)));
			String line;
			while ((line = br.readLine()) != null){
				sb.append(line);
				sb.append(System.lineSeparator());
			}
			br.close();
		}
		catch (IOException io){
			System.out.println("IO Exception at read: " + filename);
		}

		return sb.toString();
	}

	public void writeText (String text, String filename){
		try{
			BufferedWriter bw = new BufferedWriter(new FileWriter (new File (filename)));
			bw.write(text);
			bw.flush();
			bw.close();
		}
		catch(IOException io){
			System.out.println("IO Exception at write " + filename);
		}
	}

	public void writeTestFilesFromSource (String sourceName){
		String source = readText(sourceName);
		StringBuilder sb = new StringBuilder();
		try{
			BufferedReader br = new BufferedReader(new StringReader(source));
			String line;
			int fileNum = 0;
			while ((line = br.readLine())!= null){
				if (line.startsWith("Test ")){
					String [] words = line.split(" ");
					for (int i = 0; i<words.length; i++){
						if (words[i].equals("return")){
							String answer = words[i+1];
							writeText(addReturn (removeNonWord(words[i+1])), "test_answer" + (fileNum+1) + ".txt");
						}
					}
					writeText(sb.toString(), "test" + fileNum + ".txt");
					sb = new StringBuilder();
					fileNum++;
				}
				else{
					sb.append(line);
					sb.append(System.lineSeparator());
				}
			}
			writeText(sb.toString(), "test" + fileNum + ".txt");
		}
		catch(IOException io){
			System.out.println("this shouldn't happen");
		}
	}

	private String removeNonWord(String s){
		if (s.endsWith(".")){
			return s.substring(0, s.length()-1);
		}
		return s;
	}
	
	private String addReturn (String s) {
		return "return " + s + ";";
	}
	public static void main (String [] args){
		new ParseTests().writeTestFilesFromSource("source.txt");
		System.out.println("done");
	}
}
