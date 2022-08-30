unit MainFm;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, PXL.TypeDef, PXL.Types, PXL.Classes, PXL.NetComs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    SendButton: TButton;
    DestHostEdit: TEdit;
    DestPortEdit: TEdit;
    HostLabel: TLabel;
    TextEdit: TEdit;
    PortLabel: TLabel;
    PortLabel1: TLabel;
    SendGroupBox: TGroupBox;
    IncomingGroupBox: TGroupBox;
    IncomingMemo: TMemo;
    StatusBar: TStatusBar;
    SysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SysTimerTimer(Sender: TObject);
  private
    { private declarations }
    NetCom: TNetCom;
    InputStream: TMemoryStream;
    OutputStream: TMemoryStream;

    procedure OnReceiveData(const Sender: TObject; const Host: StdString; const Port: Integer; const Data: Pointer;
      const Size: Integer);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  NetCom := TNetCom.Create;

  // The following streams will be used to send/receive network data.
  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;

  // Specify the event that is going to handle data reception.
  NetCom.OnReceive := OnReceiveData;

  // Specify the local port.
  NetCom.LocalPort := 7500;

  if not NetCom.Initialize then
  begin
    NetCom.LocalPort := 0;

    if not NetCom.Initialize then
    begin
      ShowMessage('NetCom initialization failed');
      Exit;
    end;
  end;

  StatusBar.Panels[0].Text := 'Local IP: ' + NetCom.LocalIP;
  StatusBar.Panels[1].Text := 'Local Port: ' + IntToStr(NetCom.LocalPort);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  NetCom.Finalize;
  OutputStream.Free;
  InputStream.Free;
  NetCom.Free;
end;

procedure TMainForm.SendButtonClick(Sender: TObject);
var
  DestHost: StdString;
  DestPort: Integer;
begin
  // Retreive the destination host and port.
  DestHost := DestHostEdit.Text;
  DestPort := StrToIntDef(DestPortEdit.Text, -1);

  // Start with a fresh data stream.
  OutputStream.Clear;

  // Put the message text into the stream as UTF-8 StdString.
  OutputStream.PutShortString(TextEdit.Text);

  // You can use other Put[whatever] methods from StreamUtils.pas to put other
  // kind of data into the stream, like integers, floats and so on.

  // Send the data from our stream.
  NetCom.Send(DestHost, DestPort, OutputStream.Memory, OutputStream.Size);
end;

procedure TMainForm.OnReceiveData(const Sender: TObject; const Host: StdString; const Port: Integer;
  const Data: Pointer; const Size: Integer);
var
  InpText: StdString;
begin
  // Put the incoming data into our input stream.
  InputStream.Clear;
  InputStream.WriteBuffer(Data^, Size);

  // Start reading from the beginning.
  InputStream.Seek(0, soFromBeginning);

  // Read the UTF-8 StdString from the stream.
  InpText := InputStream.GetShortString;

  // You can use other Get[whatever] methods from StreamUtils.pas to get other
  // kind of data from the stream, like integers, floats and so on. Just make
  // sure that the order is exactly the same as when it was sent (see below).

  // Show the resulting text in the memo.
  IncomingMemo.Lines.Add('Received "' + InpText + '" from ' + Host + ':' + IntToStr(Port));
end;

procedure TMainForm.SysTimerTimer(Sender: TObject);
begin
  NetCom.Update;
end;

end.
