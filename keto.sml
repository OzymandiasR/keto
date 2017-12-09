structure Keto =
		struct

		datatype food = Avocado of real
			      | Burger of real
			      | Cream of real
			      | MCT of real;
		
		fun ketoMacros(yum:food list) =
		  let
		      val car_pro = 3.4
		      val fat = 9.0
		      val RtS = Real.toString
		      fun fatCal(amt:real) = fat*amt;
		      fun carproCal(amt:real) = car_pro*amt;
		      fun kMac(muy:food list,fats:real,prots:real,total:real) =
			case muy
			 of nil => print("\n\nMacros: \n\nFats: "^RtS(100.0*(fats/total))^"%\n\nProtein: "^RtS(100.0*(prots/total))^"%\n\nCrap: "^RtS(100.0*(1.0-(fats/total+prots/total)))^"%\n\nTotal: "^RtS(total)^" calories\n\nI hope you enjoyed, fat ass.\n\n")
			  | a::b => (case a
				      of Avocado(grams) => kMac(b,fats+fatCal(0.15*grams),prots+carproCal(0.02*grams),total+fatCal(0.15*grams)+carproCal(0.02*grams)+carproCal(0.09*grams))
				      |  Burger(grams) => kMac(b,fats+fatCal(0.18*grams),prots+carproCal(0.26*grams),total+fatCal(0.18*grams)+carproCal(0.26*grams))
				      |  Cream(grams) => kMac(b,fats+fatCal(0.57*grams),prots,total+fatCal(0.57*grams)+carproCal(0.01*grams))
				      |  MCT(grams) => kMac(b,fats+fatCal(grams),prots,total+fatCal(grams)))
		  in
		      kMac(yum,0.0,0.0,0.0)
		  end
		end;

val av = Keto.Avocado;
val bur = Keto.Burger;
val mct = Keto.MCT;
val cream = Keto.Cream;

val list_o_grub = [av 68.0,bur 71.0,mct 28.0,cream 36.0];

val ketoMacros = Keto.ketoMacros;

ketoMacros list_o_grub;

ketoMacros [av 68.0,bur 71.0,mct 28.0,cream 36.0];
