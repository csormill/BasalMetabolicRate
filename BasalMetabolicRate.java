import java.util.Scanner;

public class BasalMetabolicRate {
  
  private static double age;
  private static String gender;
  private static double weight;
  private static double height;
  private static double bmr;
  private static String activity;
  private static double dailyCalorieneeds = 0;
  private static double bmi = 0;
  
 /* public CalorieIntake(double age, String gender, double weight, double height, String lifestyle) {
  
    this.age = age;
    this.gender = gender;
    this.weight = weight;
    this.height = height;
    this.lifestyle = lifestyle;
  
  } */
  
  public static void main(String[] args) {
    
    Scanner in = new Scanner(System.in);
    System.out.println("What is your age? : ");
    age = Double.parseDouble(in.nextLine());
    System.out.println("What is your gender?(M or F) :  ");
    gender = in.nextLine();
    System.out.println("What is your weight?(KG) : " );
    weight = Double.parseDouble(in.nextLine());
    System.out.println("What is your height?(cm) : ");
    height = Double.parseDouble(in.nextLine());
    System.out.println("What is your daily activity factor?(Pick number)");
    System.out.println("1. Sedentary ");
    System.out.println("2. Lightly active ");
    System.out.println("3. Moderately active ");
    System.out.println("4. Very active ");
    activity = in.nextLine();
    bmi = Math.round(weight / ((height * .01) * (height * .01)));
    
    // All equations are used here : http://www.bmi-calculator.net/bmr-calculator/harris-benedict-equation/
    if (gender.equals("M")) {
     bmr = (((10 * weight) + (6.25 * height)) - (5 * age)) + 5 ;
    }else 
     bmr = (((10 * weight) + (6.25 * height)) - (5 * age)) - 161;
   System.out.println("Your Basal metabolic rate is " + bmr + " Calories / Day");
   
   if (activity.equals("1")) {
     dailyCalorieneeds = bmr * 1.2;
   }else if (activity.equals("2")) {
     dailyCalorieneeds = bmr * 1.375;
   }else if (activity.equals("3")) {
     dailyCalorieneeds = bmr * 1.55;
   }else if (activity.equals("4")) {
     dailyCalorieneeds = bmr * 1.725;
   } 
   
   if (bmi < 18.5) {
     System.out.println("Your BMI is " + bmi + " -> Underweight");
   }else if (bmi >= 18.5 && bmi <= 24.9) {
     System.out.println("Your BMI is " + bmi + " -> Normal weight");
   }else if (bmi >= 25 && bmi <= 29.9) {
     System.out.println("Your BMI is " + bmi + " -> Overweight");
   }else 
     System.out.println("Your BMI is " + bmi + " -> Obese");
     
     
   System.out.println("Your Daily Calorie needs are " + dailyCalorieneeds);
    
  }
}