unit CliUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rtcFunction, rtcDataCli, rtcCliModule, rtcInfo,
  rtcConn, rtcHttpCli;

{$include rtcDefs.inc}

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    Label1: TLabel;
    Label2: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RtcHttpClient1Connect(Sender: TRtcConnection);
    procedure RtcHttpClient1Disconnect(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
  begin
  if Key=#13 then
    begin
    Edit1.SelectAll;

    { This is the easiest way to use remote functions with RTC SDK:
      Using "Prepare" to prepare a using the "Execute" method to
      execute the remote function call, wait for the result and
      make sure the Result object is freed automatically the next
      time the "Execute" method is being used, so we do not need
      to free the result manually ... }
    with RtcClientModule1 do
      begin
      with Prepare('hello') do
        asString['name']:=Edit1.Text;
      with Execute(True,5000) do // execute remote function (free result before next call; call timeout 5000 miliseconds).
        Memo1.Lines.Add(asString); // print out the result
      end;

    { Alternatively, we could make the "Execute" call without "FALSE" as
      the 1st parameter "Execute(False)" if we wanted to keep the Result
      object for ourself and free it manually when we do not need it ... }

    (*
      var myRes:TRtcValue; // we will need a variable to hold the result object

      // Prepare the remote call ...
      Prepare('hello');
      Param.asString['name']:=Edit1.Text;

      // Execute the remote call and get the result ...
      myRes:=Execute(False); // using FALSE as "AutoFreeResult" parameter

      // we have the result, let's use it ...
      try
        Memo1.Lines.Add(myRes.asString);
      finally
        // when finished, free our result object
        myRes.Free;
        end;
      end;
    *)

    end;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  RtcHttpClient1.Disconnect;
  end;

procedure TForm1.RtcHttpClient1Connect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Connect)
  else
    Label1.Caption:='Connected';
  end;

procedure TForm1.RtcHttpClient1Disconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Disconnect)
  else
    Label1.Caption:='NOT Connected';
  end;

end.
