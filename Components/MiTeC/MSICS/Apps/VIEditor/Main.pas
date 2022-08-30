unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_VersionInfo, StdCtrls, Grids, ValEdit, Spin, ExtCtrls;

type
  Twnd_vie_Main = class(TForm)
    Label2: TLabel;
    eFile: TEdit;
    bOpen: TButton;
    od: TOpenDialog;
    vle: TValueListEditor;
    gbProd: TGroupBox;
    eProdMaj: TSpinEdit;
    Label1: TLabel;
    Label3: TLabel;
    eProdMin: TSpinEdit;
    Label4: TLabel;
    eProdRel: TSpinEdit;
    Label5: TLabel;
    eProdBuild: TSpinEdit;
    gbFile: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    eFileMaj: TSpinEdit;
    eFileMin: TSpinEdit;
    eFileRel: TSpinEdit;
    eFileBuild: TSpinEdit;
    gbAttr: TGroupBox;
    cbxDebug: TCheckBox;
    cbxPatched: TCheckBox;
    cbxSpec: TCheckBox;
    cbxPre: TCheckBox;
    cbxPriv: TCheckBox;
    gbLang: TGroupBox;
    lLID: TLabel;
    cbLang: TComboBox;
    Icon: TImage;
    bSave: TButton;
    Label10: TLabel;
    cbxBak: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure cbLangDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure cbLangChange(Sender: TObject);
    procedure eChange(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
  private
    FModified: Boolean;
    FVI: TVersionInformation;
    procedure SetModified(const Value: Boolean);
  public
    procedure OpenFile(const AFilename: string);

    property Modified: Boolean read FModified write SetModified;
  end;

var
  wnd_vie_Main: Twnd_vie_Main;

implementation

uses ShellAPI;

{$R *.dfm}

function ListIndexOfValue(AList: TStrings; const AValue: string; ASep: string = '='): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to AList.Count-1 do
    if SameText(AValue,AList.ValueFromIndex[i]) then begin
      Result:=i;
      Break;
    end;
end;

function GetFileIcon(const Filename: string; IconIndex: Word = 0): HICON;
var
  s: string;
begin
  s:=FileName;
  if ExtractIcon(HInstance,PChar(s),word(-1))>0 then
    result:=extracticon(hinstance,PChar(s),IconIndex)
  else
    result:=ExtractAssociatedIcon(HInstance,PChar(s),IconIndex);
end;

function IsFileLocked(AFilename: string): Boolean;
var
  F: TFileStream;
begin
  try
    F:=TFileStream.Create(AFilename,fmOpenReadWrite or fmShareExclusive);
    try
      Result:=False;
    finally
      F.Free;
    end;
  except
    Result:=True;
  end;
end;

function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

procedure Twnd_vie_Main.bOpenClick(Sender: TObject);
begin
  if not od.Execute then
    Exit;
  OpenFile(od.FileName);
end;

procedure Twnd_vie_Main.bSaveClick(Sender: TObject);
var
  vn: TVersionNumber;
  i: Integer;
  vr,l: Cardinal;
begin
  if IsFileLocked(FVI.FileName) then begin
    MessageDlg('File is locked. Cannot save changes.',mtWarning,[mbOK],0);
    Exit;
  end;
  Screen.Cursor:=crHourGlass;
  try
    vn.Major:=eProdMaj.Value;
    vn.Minor:=eProdMin.Value;
    vn.Release:=eProdRel.Value;
    vn.Build:=eProdBuild.Value;
    FVI.ProductVersionNumber:=vn;
    vn.Major:=eFileMaj.Value;
    vn.Minor:=eFileMin.Value;
    vn.Release:=eFileRel.Value;
    vn.Build:=eFileBuild.Value;
    FVI.FileVersionNumber:=vn;
    FVI.FileFlags:=[];
    if cbxDebug.Checked then
      FVI.FileFlags:=FVI.FileFlags+[ffDebug];
    if cbxSpec.Checked then
      FVI.FileFlags:=FVI.FileFlags+[ffSpecialBuild];
    if cbxPre.Checked then
      FVI.FileFlags:=FVI.FileFlags+[ffPreRelease];
    if cbxPriv.Checked then
      FVI.FileFlags:=FVI.FileFlags+[ffPrivateBuild];
    if cbxPatched.Checked then
      FVI.FileFlags:=FVI.FileFlags+[ffPatched];

    for i:=0 to vle.Strings.Count-1 do
      FVI.SetStringFileInfoValue(0,vle.Strings.Names[i],vle.Strings.ValueFromIndex[i]);

    l:=StrToInt(cbLang.Items.ValueFromIndex[cbLang.ItemIndex]);
    FVI.Translation:=TTranslation(MAKELONG(l,LCIDToCodePage(l)));

    if cbxBak.Checked or (MessageDlg('Do you want to backup original file?',mtConfirmation,[mbYes,mbNo],0)=mrYes) then
      CopyFile(PChar(FVI.FileName),PChar(ChangeFileExt(FVI.FileName,'.bak')),False);

    if FVI.ResourceLanguage.Lang<>FVI.Translation.Lang then begin
      vr:=BeginUpdateResource(Pointer(FVI.Filename),False);
      try
        UpdateResource(vr,RT_VERSION,MAKEINTRESOURCE(VS_VERSION_INFO),FVI.ResourceLanguage.Lang,nil,0);
      finally
        EndUpdateResource(vr,False);
      end;
    end;

    FVI.ResourceLanguage:=FVI.Translation;

    FVI.SaveData;
    OpenFile(FVI.FileName);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_vie_Main.cbLangChange(Sender: TObject);
begin
  lLID.Caption:=Format('Locale ID: $%4.4x',[StrToInt(cbLang.Items.ValueFromIndex[cbLang.ItemIndex])]);
  eChange(nil);
end;

procedure Twnd_vie_Main.cbLangDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  n,v: string;
  h: Integer;
  c: TCanvas;
begin
  if Index=-1 then
    Exit;
  with TComboBox(Control) do begin
    n:=Items.Names[Index];
    v:=Items.ValueFromIndex[Index];
    h:=ItemHeight;
    c:=Canvas;
  end;
  c.FillRect(Rect);
  TextOut(c.Handle,Rect.Left+5,Rect.Top+(h-c.TextHeight(v)) div 2,PChar(n),Length(n));
end;

procedure Twnd_vie_Main.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FVI:=TVersionInformation.Create;
  for i:=0 to Languages.Count-1 do
    cbLang.Items.Add(Format('%s=%d',[Languages.Name[i],Languages.LocaleID[i]]));
end;

procedure Twnd_vie_Main.FormShow(Sender: TObject);
begin
  if (ParamCount=1) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1));
end;

procedure Twnd_vie_Main.OpenFile(const AFilename: string);
var
  i: Integer;
begin
  eFile.Text:=AFilename;
  FVI.FileName:=AFilename;
  Icon.Picture.Icon.Handle:=GetFileIcon(AFilename);
  vle.Strings.Clear;
  for i:=0 to FVI.ItemCount-1 do
    vle.Strings.Add(Format('%s=%s',[FVI.Items[i].Name,FVI.Items[i].Value]));
  eProdMaj.Value:=FVI.ProductVersionNumber.Major;
  eProdMin.Value:=FVI.ProductVersionNumber.Minor;
  eProdRel.Value:=FVI.ProductVersionNumber.Release;
  eProdBuild.Value:=FVI.ProductVersionNumber.Build;
  eFileMaj.Value:=FVI.FileVersionNumber.Major;
  eFileMin.Value:=FVI.FileVersionNumber.Minor;
  eFileRel.Value:=FVI.FileVersionNumber.Release;
  eFileBuild.Value:=FVI.FileVersionNumber.Build;
  cbxDebug.Checked:=ffDebug in FVI.FileFlags;
  cbxPatched.Checked:=ffPatched in FVI.FileFlags;
  cbxPriv.Checked:=ffPrivateBuild in FVI.FileFlags;
  cbxPre.Checked:=ffPreRelease in FVI.FileFlags;
  cbxSpec.Checked:=ffSpecialBuild in FVI.FileFlags;
  cbLang.ItemIndex:=ListIndexOfValue(cbLang.Items,IntToStr(FVI.Translation.Lang));
  lLID.Caption:=Format('Locale ID: $%4.4x',[FVI.Translation.Lang]);
  Modified:=False;

  gbProd.Enabled:=FVI.Valid;
  gbFile.Enabled:=FVI.Valid;
  vle.Enabled:=FVI.Valid;
  gbAttr.Enabled:=FVI.Valid;
  gbLang.Enabled:=FVI.Valid;

  if vle.Enabled then
    vle.SetFocus;
end;

procedure Twnd_vie_Main.SetModified(const Value: Boolean);
begin
  FModified:=Value;
  bSave.Enabled:=FModified;
end;

procedure Twnd_vie_Main.eChange(Sender: TObject);
begin
  Modified:=True;
end;

end.
