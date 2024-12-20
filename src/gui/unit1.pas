//TODO:
//	- (wishlist) Instructions on how to sign up with Twilio
//	- (wishlist) Update availability checker tied to GitHub release page if code is released

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
	XMLPropStorage, Process, lclintf, Math, Unit2, Unit3;

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
		Label7: TLabel;
		Label8: TLabel;
		MainMenu1: TMainMenu;
		Memo1: TMemo;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		MenuItem4: TMenuItem;
		MenuItem5: TMenuItem;
		MenuItem6: TMenuItem;
		MenuItem7: TMenuItem;
		MenuItem8: TMenuItem;
		MenuItem9: TMenuItem;
		XMLConfig1: TXMLPropStorage;
		procedure update_total_cost();
		procedure Edit1Change(Sender: TObject);
		procedure Memo1Change(Sender: TObject);
		procedure MenuItem5Click(Sender: TObject);
		procedure MenuItem7Click(Sender: TObject);
		procedure MenuItem8Click(Sender: TObject);
		procedure MenuItem9Click(Sender: TObject);
		function CanConnect() : boolean;
		function SendText(Message : string; ToNumber : string) : boolean;
		function GetBalance : string;
		function CanAuthenticate() : boolean;
		function GetSirenCorePath : string;
		function NumberIsUnsubscribed(ToNumber : string) : boolean;
		procedure GetUnsubscribedNumbers();
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
		function SirenCorePath() : string;
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
	cost_per_recipient : real = 0.0079;
	number_of_recipients : integer = 4;
	unsubscribed_numbers : array of string;

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

function TForm1.SirenCorePath : string;
var
	response : string;
	slash : string;
begin
		slash := '/';
	{$if defined(windows)}
		slash := '\';
	{$ifend}
	if RunCommand('siren', ['-h'], response, [], swoHide) then
		begin
			SirenCorePath := 'siren';
		end
	else
		begin
			SirenCorePath := ExtractFilePath(Application.ExeName) + slash + 'siren';
		end;
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
			GetUnsubscribedNumbers();
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

procedure TForm1.GetUnsubscribedNumbers();
var
	OutputResult : string;
begin
	SetLength(unsubscribed_numbers, 0);
	RunCommand(SirenCorePath, ['-a', TWAccountID, '-s', TWAuthToken, '-u'], OutputResult, [], swoHide);
	OutputResult := StringReplace(OutputResult, #13#10, ',', [rfReplaceAll]);
	OutputResult := StringReplace(OutputResult, #10, ',', [rfReplaceAll]);
	unsubscribed_numbers := OutputResult.Split([',']);
end;

function TForm1.NumberIsUnsubscribed(ToNumber : string) : boolean;
var
	low, high, mid, cmp: integer;
begin
	low := 0;
	high := Length(unsubscribed_numbers) - 1;
	NumberIsUnsubscribed := false;

	while low <= high do
	begin
		mid := (low + high) div 2;
		cmp := CompareStr(unsubscribed_numbers[mid], ToNumber);

		if cmp = 0 then
		begin
			NumberIsUnsubscribed := true;
			Exit;
		end
		else if cmp < 0 then
			low := mid + 1
		else
			high := mid - 1;
	end;
end;

function TForm1.SendText(Message : string; ToNumber : string) : boolean;
var
	JunkString : string;
begin
	SendText := true;
	// Check if the number is unsubscribed
	if NumberIsUnsubscribed(ToNumber) then
	begin
		WriteLn('Number ' + ToNumber + ' is unsubscribed');
		SendText := false;
		Exit(false);
	end;

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
	RunCommand(SirenCorePath, ['-a', TWAccountID, '-s', TWAuthToken, '-f', TWFromNumber, '-t', ToNumber, '-m', Message], JunkString, [], swoHide);
	SendText := true;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
	OpenURL('https://console.twilio.com/us1/billing/manage-billing/billing-overview');
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
	ShowMessage('Siren is a free & open-source program primarily intended for use by non-profits' + #13#10 + 'It acts as a desktop frontend for Twilio, making mass texting accessible & affordable' + #13#10 + 'In order to use this program, you need a Twilio account');
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
	Application.Terminate;
end;

procedure TForm1.update_total_cost();
var
	total_cost : real;
begin
	total_cost := cost_per_recipient * number_of_recipients;
	Label7.Caption := 'Total cost: $' + FloatToStr(total_cost) + ' (approx)';
	Label8.Caption := 'Sending to ' + IntToStr(number_of_recipients) + ' phone numbers';
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
	messagelength : integer;
	messagesegments : integer;
	singlemessagecost : real = 0.0079;
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
	cost_per_recipient := singlemessagecost * messagesegments; // Update the global cost_per_recipient variable
	Label5.Caption := 'Cost: $' + FloatToStr(cost_per_recipient) + ' per recipient (approx)';
	update_total_cost();
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
	number_of_recipients := Memo1.Lines.Count;
	update_total_cost();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
	Form3.ShowModal; // Runs SendMassTexts onShow
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
var
	MemoDialog: TForm;
	Memo: TMemo;
begin
	MemoDialog := TForm.Create(nil);
	try
		MemoDialog.Width := 400;
		MemoDialog.Height := 300;
		MemoDialog.Caption := 'Unsubscribed / Invalid Numbers';

		Memo := TMemo.Create(MemoDialog);
		Memo.Parent := MemoDialog;
		Memo.Align := alClient;
		Memo.ReadOnly := true;
		Memo.ScrollBars := ssVertical;
		
		Memo.Lines.AddStrings(unsubscribed_numbers);

		MemoDialog.ShowModal;
	finally
		MemoDialog.Free;
	end;
end;

end.
