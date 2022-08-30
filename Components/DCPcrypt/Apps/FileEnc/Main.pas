{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (davebarton@bigfoot.com) *************}
{******************************************************************************}
{* Simple File Encryption Demo ************************************************}
{******************************************************************************}
{* Copyright (c) 1999-2000 David Barton                                       *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, xpman, DCPcrypt, Misty1;

type
  TMainFrm = class(TForm)
    EncryptGrp: TGroupBox;
    EncInputLbl: TLabel;
    EncInputBox: TEdit;
    EncOutputLbl: TLabel;
    EncOutputBox: TEdit;
    EncPassphrase1Box: TEdit;
    EncPassphrase2Box: TEdit;
    EncGoBtn: TButton;
    EncPassphrase1Lbl: TLabel;
    EncPassphrase2Lbl: TLabel;
    EncProgressBar: TProgressBar;
    EncAlgorithmCBx: TComboBox;
    EncAlgorithmLbl: TLabel;
    DecryptGrp: TGroupBox;
    DecInputLbl: TLabel;
    DecOutputLbl: TLabel;
    DecPassphraseLbl: TLabel;
    DecInputBox: TEdit;
    DecOutputBox: TEdit;
    DecPassphraseBox: TEdit;
    DecGoBtn: TButton;
    DecProgressBar: TProgressBar;
    BrowseBtn1: TSpeedButton;
    BrowseBtn2: TSpeedButton;
    BrowseBtn3: TSpeedButton;
    BrowseBtn4: TSpeedButton;
    OpenDlg: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    DCP_misty12: TDCP_misty1;
    DCP_misty11: TDCP_misty1;
    procedure FormCreate(Sender: TObject);
    procedure EncGoBtnClick(Sender: TObject);
    procedure DecGoBtnClick(Sender: TObject);
    procedure BrowseBtn1Click(Sender: TObject);
    procedure OpenDlgShow(Sender: TObject);
    procedure BrowseBtn2Click(Sender: TObject);
    procedure BrowseBtn3Click(Sender: TObject);
    procedure BrowseBtn4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrm                   : TMainFrm;

implementation

{ In the uses we put all of the ciphers we want to have available - they will
  automatically register themselves with DCPcrypt }
uses
  Blowfish, Cast128, DES, Gost, Ice, RC2, RC4, Rijndael,
  Twofish;

{$R *.DFM}

procedure TMainFrm.FormCreate(Sender: TObject);
var
  Ciphers                   : PDCP_cipherinfo;
begin
  Ciphers := DCPcipherlist;             { A pointer to the first record in the list of ciphers }
  while Ciphers <> nil do
  begin
    { Add the cipher and it's class to the combobox - alternatively it we could
      just add the cipher name and lookup it's class from its name later using
      the DCPcipherfromalgorithm function }
    EncAlgorithmCBx.Items.AddObject(Ciphers^.Cipher.GetAlgorithm, TObject(Ciphers^.Cipher));
    Ciphers := Ciphers^.Next;           { Cycle through all the available ciphers }
  end;
  EncAlgorithmCBx.ItemIndex := 0;       { An set the default cipher to the first one in the list }
end;

procedure TMainFrm.EncGoBtnClick(Sender: TObject);
var
  Cipher                    : TDCP_cipher;
  Input, Output             : TFileStream;
  Progress, Done            : longint;
  Header                    : packed record { This is the header of the encrypted file }
    Id: longint;
    Check1, Check2: longint;
  end;
begin
  { Make sure that the password is what the user thinks it is }
  if EncPassphrase1Box.Text <> EncPassphrase2Box.Text then
  begin
    MessageDlg('Passphrases are different', mtError, [mbOK], 0);
    EncPassphrase1Box.SetFocus;
    Exit;
  end
    { And make sure that there is a passphrase entered }
  else if EncPassphrase1Box.Text = '' then
  begin
    MessageDlg('Please enter a passphrase', mtInformation, [mbOK], 0);
    EncPassphrase1Box.SetFocus;
    Exit;
  end
    { And make sure that the file exists }
  else if not FileExists(EncInputBox.Text) then
  begin
    MessageDlg('Input file doesn''t exist', mtError, [mbOK], 0);
    EncInputBox.SetFocus;
    Exit;
  end
    { Check we don't overwrite a file we're not supposed to }
  else if FileExists(EncOutputBox.Text) then
    if MessageDlg('Output file already exists - Overwrite?', mtConfirmation,
      mbYesNoCancel, 0) <> mrYes then
      Exit;
  { Create the cipher from the class we stored in the combobox }
  Cipher := TDCP_cipherclass(EncAlgorithmCBx.Items.Objects[EncAlgorithmCBx.ItemIndex]).Create(Self);
  { Alternatively we could use
    Cipher:= DCPcipherfromalgorithm(EncAlgorithmCBx.Items[EncAlgorithmCBx.ItemIndex],Self);
    which would create the cipher from its algorithm name }

  try
    Input := TFileStream.Create(EncInputBox.Text, fmOpenRead); { Open the input file }
  except
    Cipher.Free;
    MessageDlg('Unable to open input file', mtError, [mbOK], 0);
    Exit;
  end;

  try
    Output := TFileStream.Create(EncOutputBox.Text, fmCreate); { Open the output file }
  except
    Cipher.Free;
    Input.Free;
    MessageDlg('Unable to create output file', mtError, [mbOK], 0);
    Exit;
  end;
  Progress := 0;                        { Progress contains the total bytes done }
  Header.Id := Cipher.Id; { Store the cipher id so we know what algorithm to use
  on decrypting }
  Header.Check1 := Random($FFFF) or (Random($FFFF) shl 16);
  Header.Check2 := Header.Check1;
  { To check that we have the right passphrase on decryption if we store a
    random number twice then when we decrypt we know that the two numbers
    should match }
  Cipher.InitStr(EncPassphrase1Box.Text); { Initialize the cipher using the passphrase }
  if Cipher is TDCP_blockcipher then
    TDCP_blockcipher(Cipher).CipherMode := cmCFBblock; { Set the appropriate mode if a block cipher }
  Cipher.Encrypt(Header.Check1, Header.Check1, 8); { Encrypt the last 8 bytes of header - NOT the id }
  Output.Write(Header, SizeOf(Header));
  repeat
    Done := Cipher.EncryptStream(Input, Output, 32768); { Encrypt the file 32768 bytes at a time }
    Inc(Progress, Done);
    EncProgressBar.Position := (Progress * 100) div Input.Size; { Update the progress bar }
  until Done <> 32768;
  Input.Free;                           { Free all resources used }
  Output.Free;
  Cipher.Burn;                          { Remember to Burn after use }
  Cipher.Free;
  FillChar(Header, SizeOf(Header), 0);
  MessageDlg('File encrypted sucessfully', mtInformation, [mbOK], 0);
  EncProgressBar.Position := 0;
end;

procedure TMainFrm.DecGoBtnClick(Sender: TObject);
var
  Cipher                    : TDCP_cipher;
  Input, Output             : TFileStream;
  Progress, Done            : longint;
  Header                    : packed record
    Id: longint;
    Check1, Check2: longint;
  end;
begin
  if not FileExists(DecInputBox.Text) then begin
    MessageDlg('Input file doesn''t exist', mtError, [mbOK], 0);
    DecInputBox.SetFocus;
    Exit;
  end else if FileExists(DecOutputBox.Text) then
    if MessageDlg('Output file already exists - Overwrite?', mtConfirmation, mbYesNoCancel, 0) <> mrYes then
      Exit;

  try
    Input := TFileStream.Create(DecInputBox.Text, fmOpenRead);
  except
    MessageDlg('Unable to open input file', mtError, [mbOK], 0);
    Exit;
  end;

  try
    Output := TFileStream.Create(DecOutputBox.Text, fmCreate);
  except
    Input.Free;
    MessageDlg('Unable to create output file', mtError, [mbOK], 0);
    Exit;
  end;

  Input.Read(Header, SizeOf(Header));
  Cipher := DCPcipherfromid(Header.Id, Self); { Create the cipher based on the id stored in the file }
  Progress := 0;
  Cipher.InitStr(DecPassphraseBox.Text); { Initialize the cipher with the passphrase }

  if Cipher is TDCP_blockcipher then
    TDCP_blockcipher(Cipher).CipherMode := cmCFBblock; { Set the mode }

  Cipher.Decrypt(Header.Check1, Header.Check1, 8); { Decrypt the header and check that the two numbers we stored are the same }
  if Header.Check1 <> Header.Check2 then begin
    Input.Free;
    Output.Free;
    DeleteFile(DecOutputBox.Text);
    Cipher.Burn;
    Cipher.Free;
    MessageDlg('Incorrect passphrase', mtInformation, [mbOK], 0);
    Exit;
  end;

  repeat
    Done := Cipher.DecryptStream(Input, Output, 32768); { Decrypt the file }
    Inc(Progress, Done);
    DecProgressBar.Position := (Progress * 100) div Input.Size;
  until Done <> 32768;

  Input.Free;
  Output.Free;
  Cipher.Burn;                          { Remember to Burn }
  Cipher.Free;
  FillChar(Header, SizeOf(Header), 0);
  MessageDlg('File decrypted sucessfully', mtInformation, [mbOK], 0);
  DecProgressBar.Position := 0;
end;

procedure TMainFrm.OpenDlgShow(Sender: TObject);
begin
  OpenDlg.InitialDir := ExtractFileDir(OpenDlg.Filename);
end;

procedure TMainFrm.BrowseBtn1Click(Sender: TObject);
begin
  if OpenDlg.Execute then
    EncInputBox.Text := OpenDlg.Filename;
end;

procedure TMainFrm.BrowseBtn2Click(Sender: TObject);
begin
  if OpenDlg.Execute then
    EncOutputBox.Text := OpenDlg.Filename;
end;

procedure TMainFrm.BrowseBtn3Click(Sender: TObject);
begin
  if OpenDlg.Execute then
    DecInputBox.Text := OpenDlg.Filename;
end;

procedure TMainFrm.BrowseBtn4Click(Sender: TObject);
begin
  if OpenDlg.Execute then
    DecOutputBox.Text := OpenDlg.Filename;
end;

type
  TRcHeader = packed record
    sCompany: string[050];              // = 'ÈÈÑª´«Ææ';
    sFileName: string[050];             // = 'ÈÈÑª´«ÆæµÇÂ½Æ÷';
    sWebLink: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41359108DC0D5C25228D808D71F2E82078';
    sWebSite: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A4167BEFB55C9F9AD2F';
    sBbsSite: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A4167BEFB55C9F9AD2F';

    sListSite: string[255];
    GameList: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41555B8ECAE49841B72C28D608615BB0E306F0C168FEC29FAECAFD4FDDAAE82DD8';
    GameList2: string[255];
    sSiteUrl: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41555B8ECAE49841B74F398006846553C8AA3E4F7A4F6BDEAD';
    sPathAdress: array[1..3] of string[198];
    nPicSize: Integer;
  end;
  pTRcHeader = ^TRcHeader;

  THeader = packed record
    Id: longint;
    Check1: longint;
    Check2: longint;
  end;

procedure TMainFrm.Button1Click(Sender: TObject);
var
  Cipher                    : TDCP_cipher;
  RcHeader                  : TRcHeader;
  c, Done, fHandle          : Integer;
  Input                     : TMemoryStream;
  Output                    : TFileStream;
  Header                    : THeader;
begin
  c := 0;
  try
    FillChar(RcHeader, SizeOf(RcHeader), 0);
    RcHeader.sCompany := 'ÈÈÑª´«Ææ';
    RcHeader.sFileName := 'ÈÈÑª´«ÆæµÇÂ½Æ÷';
    RcHeader.sWebLink := 'http://www.92th.com';
    RcHeader.sBbsSite := 'http://www.92th.com';
    RcHeader.sListSite := 'http://www.92th.com/LoginTool/List.txt';
    RcHeader.GameList := 'http://www.92th.com/LoginTool/SList.txt';
    RcHeader.GameList2 := 'http://www.92th.com/LoginTool/SList2.txt';
    RcHeader.sSiteUrl := 'http://www.92th.com';
    RcHeader.nPicSize := $FFFF;

    {Cipher := TDCP_misty1.Create(Self);
    //Cipher := TDCP_cipherclass(EncAlgorithmCBx.Items.Objects[EncAlgorithmCBx.ItemIndex]).Create(Self);

    c := 1;
    Input := TMemoryStream.Create();
    c := 2;
    Output := TFileStream.Create('.\1.bin', fmCreate);
    c := 3;
    Input.WriteBuffer(RcHeader, SizeOf(TRcHeader));

    c := 33;
    Input.Seek(0, 0);

    c := 4;
    Header.Id := Cipher.Id;
    Header.Check1 := Random($FFFF) or (Random($FFFF) shl 16);
    Header.Check2 := Header.Check1;

    c := 5;
    Cipher.InitStr('123');
    if Cipher is TDCP_blockcipher then TDCP_blockcipher(Cipher).CipherMode := cmCFBblock;
    Cipher.Encrypt(Header.Check1, Header.Check1, 8);

    c := 6;
    Output.Write(Header, SizeOf(Header));
    repeat
      Done := Cipher.EncryptStream(Input, Output, 32768);
    until Done <> 32768;
    Input.Free;
    Output.Free;
    Cipher.Burn;
    Cipher.Free;
    FillChar(Header, SizeOf(Header), 0);}

    DCP_misty11.InitStr('12345');
    DCP_misty11.Reset;
    DCP_misty11.Encrypt(RcHeader, RcHeader, SizeOf(TRcHeader));
    fHandle := FileCreate('.\1.bin', fmOpenWrite);
    FileWrite(fHandle, RcHeader, SizeOf(TRcHeader));
    FileClose(fHandle);
  except
    MessageDlg('exception ' + IntToStr(c), mtInformation, [mbOK], 0);
  end;
end;

procedure TMainFrm.Button2Click(Sender: TObject);
var
  RcHeader                  : TRcHeader;
  Done, fHandle             : Integer;
  Input, Output             : TFileStream;
  Header                    : THeader;
  Cipher                    : TDCP_cipher;
begin
  {Input := TFileStream.Create('.\1.bin', fmOpenRead);
  Output := TFileStream.Create('.\2.bin', fmCreate);

  Input.Read(Header, SizeOf(Header));
  Cipher := DCPcipherfromid(Header.Id, Self);

  Cipher.InitStr('123');

  if Cipher is TDCP_blockcipher then
    TDCP_blockcipher(Cipher).CipherMode := cmCFBblock;

  Cipher.Decrypt(Header.Check1, Header.Check1, 8);
  if Header.Check1 <> Header.Check2 then begin
    Input.Free;
    Output.Free;
    Cipher.Burn;
    Cipher.Free;
    MessageDlg('Incorrect passphrase', mtInformation, [mbOK], 0);
    Exit;
  end;

  repeat
    Done := Cipher.DecryptStream(Input, Output, 32768);
  until Done <> 32768;

  Input.Free;
  Output.Free;
  Cipher.Burn;
  Cipher.Free;
  FillChar(Header, SizeOf(Header), 0);}
  fHandle := FileOpen('.\1.dat', fmOpenRead);
  FileRead(fHandle, RcHeader, SizeOf(TRcHeader));
  FileClose(fHandle);
  DCP_misty12.InitStr('blue');
  DCP_misty12.Reset;
  DCP_misty12.Decrypt(RcHeader, RcHeader, SizeOf(TRcHeader));
  fHandle := FileCreate('.\2.dat', fmOpenWrite);
  FileWrite(fHandle, RcHeader, SizeOf(TRcHeader));
  FileClose(fHandle);
end;

initialization
  Randomize;

end.

