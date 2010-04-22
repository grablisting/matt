package Options is
	
	type OptionName is
		(
			SelectionSort,
			InsertionSort,
			ShannonSort,
			ShellSort,
			Quit,
			InvalidOption
		);

	procedure DisplayOptions;

	function GetOption return OptionName;

end Options;
			

