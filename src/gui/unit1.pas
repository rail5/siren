//TODO:
//	- (wishlist) Instructions on how to sign up with Twilio
//	- (wishlist) Update availability checker tied to GitHub release page if code is released

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
	XMLPropStorage, Process, Math, Unit2, Unit3;

type

	{ TForm1 }

	TForm1 = class(TForm)
		Button1: TButton;
		Edit1: TMemo;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
                Label5: TLabel;
                Label6: TLabel;
		MainMenu1: TMainMenu;
		Memo1: TMemo;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		MenuItem4: TMenuItem;
		MenuItem5: TMenuItem;
		MenuItem6: TMenuItem;
		MenuItem7: TMenuItem;
		XMLConfig1: TXMLPropStorage;
                procedure Edit1Change(Sender: TObject);
  procedure MenuItem5Click(Sender: TObject);
		procedure MenuItem7Click(Sender: TObject);
		function CanConnect() : boolean;
		function SendText(Message : string; ToNumber : string) : boolean;
		function GetBalance : string;
		function CanAuthenticate() : boolean;
		function GetSirenCorePath : string;
		procedure UpdateMainScreenInfo;
		procedure MarkNotAuthenticated;
		procedure MarkAuthenticated;
		procedure UpdateData(var accountidinput: string; authtokeninput: string; sidinput: string);
		procedure Button1Click(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure MenuItem2Click(Sender: TObject);
		procedure StoreFormState;
		procedure RestoreFormState;
	private

	public

	end;

var
	Form1: TForm1;
	TWAccountID : string;
	TWAuthToken : string;
	TWFromNumber : string;
	TWInfoIsFilledOut : boolean;
	TWInfoIsAuthenticated : boolean;
	TWBalance : string;
	SirenCorePath : string;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.GetSirenCorePath : string;
begin
	GetSirenCorePath := SirenCorePath;
end;

procedure TForm1.StoreFormState;
begin
	XMLConfig1.StoredValue['TWAccountID'] := TWAccountID;
	XMLConfig1.StoredValue['TWAuthToken'] := TWAuthToken;
	XMLConfig1.StoredValue['TWFromNumber'] := TWFromNumber;

	XMLConfig1.Save;
end;

procedure TForm1.RestoreFormState;
begin
	XMLConfig1.Restore;

	TWAccountID := XMLConfig1.StoredValue['TWAccountID'];
	TWAuthToken := XMLConfig1.StoredValue['TWAuthToken'];
	TWFromNumber := XMLConfig1.StoredValue['TWFromNumber'];

	WriteLn(XMLConfig1.StoredValue['TWAccountID']);
end;

procedure TForm1.MarkNotAuthenticated;
begin
	TWInfoIsAuthenticated := false;
	Form2.MarkSettingsNotAuthenticated;
end;

procedure TForm1.MarkAuthenticated;
begin
	TWInfoIsAuthenticated := true;
	Form2.MarkSettingsAuthenticated;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	StoreFormState;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	XMLConfig1.Filename := GetUserDir + '/siren-config.xml';
	RestoreFormState;

	TWInfoIsFilledOut := ((TWAccountID <> '') and (TWAuthToken <> '') and (TWFromNumber <> ''));

	TWInfoIsAuthenticated := false;

	SirenCorePath := ExtractFilePath(Application.ExeName) + '/siren';

	if (not CanConnect()) then
		begin
			ShowMessage('Error: Can''t connect to Twilio. Is your computer online?');
		end;

	if (not TWInfoIsFilledOut) then
	begin
		MarkNotAuthenticated;
		ShowMessage('Notice: You are not signed in.' + #13#10 + 'Please enter your Twilio account information in the "Settings" menu');
	end;

	if (TWInfoIsFilledOut) then
	begin
	     TWInfoIsAuthenticated := CanAuthenticate();

		if (not TWInfoIsAuthenticated) then
		begin
			MarkNotAuthenticated;
			ShowMessage('Notice: Your Twilio account info is invalid.' + #13#10 + 'Please verify that your settings match your Twilio account information');
		end;

		if (TWInfoIsAuthenticated) then
		begin
			MarkAuthenticated;
			UpdateMainScreenInfo;
		end;
	end;

end;

procedure TForm1.UpdateData(var accountidinput: string; authtokeninput: string; sidinput: string);
begin
	TWAccountID := accountidinput;
	TWAuthToken := authtokeninput;
	TWFromNumber := sidinput;
	StoreFormState;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
	Form2.ShowModal();
end;

function TForm1.CanAuthenticate : boolean;
var
	OutputResult : string;
begin
	RunCommand(SirenCorePath, ['-a', TWAccountID, '-s', TWAuthToken, '-c'], OutputResult, [], swoHide);
	CanAuthenticate := (OutputResult = '1');
end;

function TForm1.GetBalance : string;
var
	OutputBalance : string;
begin
	if (not TWInfoIsAuthenticated) then
	begin
		GetBalance := '0';
		Exit('0');
	end;

	OutputBalance := 'Error';

	// Run Command ( 'siren-core', check balance args)

	RunCommand(SirenCorePath, ['-a', TWAccountID, '-s', TWAuthToken, '-b'], OutputBalance, [], swoHide);

	GetBalance := OutputBalance;
end;

procedure TForm1.UpdateMainScreenInfo;
begin
	if (TWInfoIsAuthenticated) then
	begin
		Label2.Caption := 'You are signed in';
		Label3.Caption := 'Balance: $' + GetBalance();
	end;

	if (not TWInfoIsAuthenticated) then
	begin
		Label2.Caption := 'You are not signed in';
		Label3.Caption := 'Balance: $0';
	end;
end;

function TForm1.CanConnect() : boolean;
var
	ReturnValue : string;
begin

	if (RunCommand(SirenCorePath, ['-i'], ReturnValue, [], swoHide)) then
	begin
	   CanConnect := (ReturnValue = '1');
	end
	else
	begin
	    ShowMessage('Fatal error. Please re-install Siren');
	    CanConnect := false;
	end;
end;

function TForm1.SendText(Message : string; ToNumber : string) : boolean;
var
	JunkString : string;
begin


	// By law, automated text messages should give instructions on how to unsubscribe
	// the #13#10 is \r\n
	if (Message.EndsWith('(stop=quit)', true) <> true) then
	begin
		if (Message.EndsWith('stop=quit', true) <> true) then
		begin
			Message := Message + #13#10 + '(stop=quit)';
		end
	end;

	// RunCommand ('siren-core', send text args)
	SendText := RunCommand(SirenCorePath, ['-a', TWAccountID, '-s', TWAuthToken, '-f', TWFromNumber, '-t', ToNumber, '-m', Message], JunkString, [], swoHide);

end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
	ShowMessage('Siren is a free & open-source program primarily intended for use by non-profits' + #13#10 + 'It acts as a desktop frontend for Twilio, making mass texting accessible & affordable' + #13#10 + 'In order to use this program, you need a Twilio account');
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
	Application.Terminate;
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
	messagelength : integer;
	messagesegments : integer;
	singlemessagecost : real = 0.0079;
	costperrecipient : real;
begin
	messagelength := Length(Edit1.Text);
	// The maximum length of a single SMS message is 160 characters.
	// In the event that we try to send a longer message,
	// the message is split into segments of 153 characters each
	// (153 for each message, reserving the other 7 for a message header
	// which specifies which segment this is, and how to reassemble the
	// full message)
	// Twilio will charge for each message segment, and the user should
	// be informed
	messagesegments := Ceil(messagelength / 153);
	costperrecipient := singlemessagecost * messagesegments;
	Label5.Caption := 'Cost: $' + FloatToStr(costperrecipient) + ' per recipient (approximately)';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
	Form3.ShowModal; // Runs SendMassTexts onShow
end;

end.
