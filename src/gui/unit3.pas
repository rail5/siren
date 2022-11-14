unit Unit3;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
	ExtCtrls, math;

type

	{ TForm3 }

	TForm3 = class(TForm)
		Button1: TButton;
		Label1: TLabel;
		Label2: TLabel;
		ProgressBar1: TProgressBar;
		Timer1: TTimer;
		procedure Button1Click(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure SendMassTexts(var phonelist: TStrings; message : string);
		procedure Timer1Timer(Sender: TObject);
	private

	public

	end;

var
	Form3: TForm3;
	KillIt : boolean=false;
	Gone : boolean=false;

implementation
uses Unit1;

{$R *.lfm}

{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
begin
	// Cancel sending
	KillIt := true;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TForm3.SendMassTexts(var phonelist: TStrings; message: string);
var
	i : integer=0;
	NumberAndName : TStringList=nil;
	PhoneNumber : string;
	FirstName : string;
	newmessage : string='';
	ProgStep : real=1;
	counter : integer=1;
	CounterString : string;
	ListCountString : string;
begin
	ProgressBar1.Position := 0;
	Label2.Caption := '';

	ListCountString := IntToStr(phonelist.Count);
	Application.ProcessMessages();
	while (i < phonelist.Count) do
	begin
		Application.ProcessMessages();
		if (KillIt = true) then
		begin
			i := 0;
			KillIt := false;
			ProgressBar1.Position := 0;
			Label2.Caption := 'Cancelled';
			ShowMessage('Cancelled');
			Gone := false;
			Form3.Close;
			Exit;
			break;
		end;
		ProgStep := (100 / phonelist.Count);
		if (phonelist.Strings[i] = '') then
		begin
			inc(i);
			continue; // Skip empty lines in the phone list
		end;
		PhoneNumber := '0';
		FirstName := '';
		NumberAndName := TStringList.Create;
		NumberAndName.Delimiter := ',';
		NumberAndName.DelimitedText := phonelist.Strings[i]; // Split each line from phone list into an array, separated by commas

		PhoneNumber := NumberAndName[0];

		counter := i + 1;
		CounterString := IntToStr(counter);
		Label2.Caption := 'Sending to ' + PhoneNumber + ' (' + CounterString + ' of ' + ListCountString + ')';
		ProgressBar1.Position := Ceil(ProgressBar1.Position + ProgStep);
		Update();
		Application.ProcessMessages();

		if (NumberAndName.Count > 1) then
		begin
			FirstName := NumberAndName[1]; // Set FirstName to value if specified in the list
		end;

		// Replace occurrences of "{name}" (case-insensitive) with recorded FirstName
		//newmessage := '(to ' + PhoneNumber + ') ' + StringReplace(message, '{name}', FirstName, [rfReplaceAll, rfIgnoreCase]);
		newmessage := StringReplace(message, '{name}', FirstName, [rfReplaceAll, rfIgnoreCase]);

		//WriteLn(newmessage);

		Form1.SendText(newmessage, PhoneNumber);

		// Free up memory
		NumberAndName.Free;
		inc(i);
		Sleep(7000); // Sleep 7 seconds to avoid getting carrier-filtered for sending too many messages via longcodes
		// It is strongly recommended that this value is not changed, otherwise people may not receive your messages
	end;

	ProgressBar1.Position := 100;
	Label2.Caption := 'Finished';

	ShowMessage('Done!');

	Gone := false;


	Form3.Close;
	Exit;

end;

procedure TForm3.Timer1Timer(Sender: TObject);
var
   Message : string;
   phonelist : TStrings;
begin
     phonelist := Form1.Memo1.Lines;
     Message := Form1.Edit1.Text;
     Timer1.Enabled := false;
     Gone := true;
     SendMassTexts(phonelist, Message);
end;

end.

