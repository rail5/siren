unit Unit2;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process;

type

	{ TForm2 }

	TForm2 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Edit1: TEdit;
		Edit2: TEdit;
		Edit3: TEdit;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		procedure MarkSettingsAuthenticated;
		procedure MarkSettingsNotAuthenticated;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private

	public

	end;

var
	Form2: TForm2;
	SettingsAuthenticate : boolean;

implementation

uses Unit1;

{$R *.lfm}

{ TForm2 }

procedure TForm2.MarkSettingsAuthenticated;
begin
	SettingsAuthenticate := true;
end;

procedure TForm2.MarkSettingsNotAuthenticated;
begin
	SettingsAuthenticate := false;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
	TWAccountID : string;
	TWAuthToken : string;
	TWFromNumber : string;
begin
	TWAccountID := Edit1.Text;
	TWAuthToken := Edit2.Text;
	TWFromNumber := Edit3.Text;
	Form1.UpdateData(TWAccountID, TWAuthToken, TWFromNumber);

	if (SettingsAuthenticate) then
	begin
		Form1.MarkAuthenticated;
	end;

	if (not SettingsAuthenticate) then
	begin
		Form1.MarkNotAuthenticated;
	end;

	Form2.Close();
	Exit;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
	result : boolean = false;
	coreresponse : string;
begin
	// Do auth check here

	if RunCommand(Form1.GetSirenCorePath, ['-a', Edit1.Text, '-s', Edit2.Text, '-f', Edit3.Text, '-c'], coreresponse, [], swoHide) then
	  result := (coreresponse = '1')
	else
	  ShowMessage('Error: Is Siren properly installed?');

	if (result) then
	begin
		SettingsAuthenticate := true; //Mark that these settings work
		ShowMessage('Settings OK! Remember to save them.');
	end;

	if (not result) then
	begin
		SettingsAuthenticate := false; //Vice versa
		ShowMessage('Something didn''t work.' + #13#10 + #13#10 + 'Please verify that your settings match your Twilio account information');
	end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
	Edit1.Text := Form1.XMLConfig1.StoredValue['TWAccountID'];
	Edit2.Text := Form1.XMLConfig1.StoredValue['TWAuthToken'];
	Edit3.Text := Form1.XMLConfig1.StoredValue['TWFromNumber'];
end;

end.

