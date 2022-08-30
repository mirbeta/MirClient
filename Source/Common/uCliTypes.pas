unit uCliTypes;

{$I M2.inc}

interface
  uses Windows, Messages, Classes, SysUtils, Variants, Forms, AdvShapeButton, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB,
  W7Classes, W7ProgressBars, ComCtrls, StdCtrls, ExtCtrls, jpeg, gifimg, PNGIMAGE, Graphics, NativeXmlObjectStorage, Controls,
  uEDCode, ZLib;

type
  IuApplication = interface
    ['{8DD07DB3-264D-437F-8F02-D05E0473C58C}']
    procedure StartGame;
    procedure Close;
    procedure RegisterID;
  //  procedure FPLOnChange;
    procedure ChangePassWord;
    procedure GetBackPassWord;
    procedure OpenURL(const Url: String);
    procedure Config;
  end;

  TuImageButton = class(TAdvCustomShapeButton)
  published
    property Picture;
    property PictureHot;
    property PictureDown;
    property PictureDisabled;
  end;

  TuURLKind = (ukCustom, ukHomePage, ukPayPage, ukContactPage);
  TuURLButton = class(TuImageButton)
  private
    FURL: String;
    FURLKind: TuURLKind;
  protected
    procedure Click; override;
  published
    property URL: String read FURL write FURL;
    property URLKind: TuURLKind read FURLKind write FURLKind;
  end;

  TuStartBtton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuMinimizeButton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuCloseButton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuConfigButton  = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuRegisterButton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuChangePassButton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuGetBackPassButton = class(TuImageButton)
  protected
    procedure Click; override;
  end;

  TuWebBrowser = class(TEmbeddedWB)
  public
    constructor Create(Owner: TComponent); override;
  end;

  TuCurProgressBar = class(TW7CustomProgressBar)
  published
    property BackgroundColor;
    property Transparent;
    property Style;
    property Colors;
  end;

  TuAllProgressBar = class(TW7CustomProgressBar)
  published
    property BackgroundColor;
    property Transparent;
    property Style;
    property Colors;
  end;

  TuLable = class(TCustomLabel)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Font;
  end;

  TuInfLable = class(TCustomLabel)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Font;
  end;

  TuImage = class(TImage)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
  end;

  TuTreeView = class(TCustomTreeView)
  public
    constructor Create(AOwner: TComponent); override;
    property Items;
    property Images;
  published
    property Align;
    property Color;
    property Font;
    property TabOrder;
    property OnChange;
    property OnChanging;
    property OnAdvancedCustomDrawItem;
    property OnMouseDown;
  end;

  TuServerCombobox = class(TCustomComboBox)
  public
    constructor Create(AOwner: TComponent); override;
    property OnChange;
  published
    property Align;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Color;
    property Font;
    property TabOrder;
  end;

  TuFplCombobox = class(TCustomComboBox)
  public
    constructor Create(AOwner: TComponent); override;
    property OnChange;
 // protected
  //  procedure Change; override;
  published
    property Align;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Color;
    property Font;
    property TabOrder;
  end;


  TEmbeddedFileItem = class(TCollectionItem)
  private
    FFileName: String;
    FPath: String;
    FZLib: Boolean;
    FReplace: Boolean;
  published
    property Path: String read FPath write FPath;
    property FileName: String read FFileName write FFileName;
    property ZLib: Boolean read FZLib write FZLib;
    property Replace: Boolean read FReplace write FReplace;
  end;

  TEmbeddedFiles = class(TCollection)
  private
    function Get(index: Integer): TEmbeddedFileItem;
  public
    function Add: TEmbeddedFileItem;
    property Items[index: Integer]: TEmbeddedFileItem read Get; default;
  end;

  TuCustomForm = class(TForm)
  private
    FEmbeddedFiles: TEmbeddedFiles;
    FWindowPosChanged: TNotifyEvent;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnWindowPosChanged: TNotifyEvent read FWindowPosChanged write FWindowPosChanged;
  published
    property EmbeddedFiles: TEmbeddedFiles read FEmbeddedFiles write FEmbeddedFiles;
  end;

  TuForm = class(TuCustomForm)
  public
    property Position;
  published
    property EmbeddedFiles;
    property PixelsPerInch;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property TransparentColor default False;
    property TransparentColorValue default 0;
    property OnMouseDown;
    property OnClose;
  end;

  TFormHelper = class helper for TForm
  private
    procedure ReadError(Reader: TReader; const Message: string; var Handled: Boolean);
  public
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
  end;

  TuExecutableStructure = record
    Title: String[100];
    ServerListURL: String[255];
    ServerListURL2: String[255];
    ServerListURL3: String[255];
    Shield:Boolean;
    ShieldUrl: String[255];
    ShieldUrl2: String[255];
    SkinDataLen: Integer;
    Offset: Integer;
  end;

//  TuExecutableStructure = record
//    Title: String[100];
//    ServerListURL: String[150];
//    SkinDataLen: Integer;
//    Offset: Integer;
//  end;

  TEmbeddedFile = record
    Path: String[200];
    FileName: String[200];
    ZLib: Boolean;
    Replace: Boolean;
    Size: Cardinal;
  end;
  PTEmbeddedFile = ^TEmbeddedFile;

  TuExecutableHeader = record
    RecordID: String[100];
    Title: String[200];
    ServerListURL: array[0..1023] of AnsiChar;
    ServerListURL1: array[0..1023] of AnsiChar;
    ServerListURL2: array[0..1023] of AnsiChar;
  end;

function CreateCliFrom(AOwner: TComponent): TuForm;

var
  _Application: IuApplication = nil;

implementation

function CreateCliFrom(AOwner: TComponent): TuForm;
begin
  Result  :=  TuForm.Create(AOwner);
  Result.Name :=  'LoginForm';
  Result.BorderStyle  :=  bsNone;
end;

{ TuCloseButton }

procedure TuCloseButton.Click;
begin
  inherited;
  if _Application<>nil then
    _Application.Close
  else
    if Owner is TCustomForm then
    TCustomForm(Owner).Close;
end;

{TuMinimizeButton}

procedure TuMinimizeButton.Click;
begin
  inherited;
  Application.Minimize;
end;

procedure TFormHelper.LoadFromFile(const FileName: String);
var
  AFile: TFileStream;
begin
  AFile :=  TFileStream.Create(FileName, fmOpenRead);
  try
    AFile.Seek(0, soFromBeginning);
    LoadFromStream(AFile);
  finally
    AFile.Free;
  end;
end;

procedure TFormHelper.LoadFromStream(AStream: TStream);
var
  Temp, ACompStream: TStream;
  Reader: TReader;
begin

  Temp        :=  TMemoryStream.Create;
  ACompStream :=  TMemoryStream.Create;
  Reader := TReader.Create(Temp, 4096);
  try
    Reader.OnError := ReadError;
    uEDCode.DecodeStream(AStream, ACompStream, '2D55E363-3057-4474-ADC0-00A3E31C3BF6');
    ACompStream.Seek(0, soFromBeginning);
    ZLib.ZDecompressStream(ACompStream, Temp);
    Temp.Seek(0, soFromBeginning);
    Reader.ReadRootComponent(Self);
  finally
    Reader.Free;
    FreeAndNil(ACompStream);
    FreeAndNil(Temp);
  end;

end;

procedure TFormHelper.ReadError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TFormHelper.SaveToFile(const FileName: String);
var
  AFile: TFileStream;
begin
  AFile :=  TFileStream.Create(FileName, fmCreate);
  try
    AFile.Seek(0, soFromBeginning);
    SaveToStream(AFile);
  finally
    AFile.Free;
  end;
end;

procedure TFormHelper.SaveToStream(AStream: TStream);
var
  ATemp, ACompStream: TMemoryStream;
  OldVisible: Boolean;
begin

  ATemp       :=  TMemoryStream.Create;
  ACompStream :=  TMemoryStream.Create;
  try
    Position := poDesktopCenter;
    ATemp.WriteComponent(Self);
    ATemp.Seek(0, soBeginning);
    OldVisible := Visible;
    Visible := False;
    ZLib.ZCompressStream(ATemp, ACompStream);
    ACompStream.Seek(0, soBeginning);
    uEDCode.EncodeStream(ACompStream, AStream, '2D55E363-3057-4474-ADC0-00A3E31C3BF6');
    AStream.Seek(0, soBeginning);
  finally
    Visible := OldVisible;
    ATemp.Free;
    ACompStream.Free;
  end;

end;

{ TuURLButton }

procedure TuURLButton.Click;
begin
  inherited;
  Enabled :=  False;
  try
    if _Application <> nil then
      _Application.OpenURL(FURL);
  finally
    Enabled :=  True;
  end;
end;

{ TuStartBtton }

procedure TuStartBtton.Click;
begin
  inherited;
  Enabled :=  False;
  try
    if _Application <> nil  then
      _Application.StartGame;
  finally
    Enabled :=  True;
  end;
end;

{ TuConfigButton }

procedure TuConfigButton.Click;
begin
  inherited;
  if _Application<>nil then
    _Application.Config;
end;

{ TuRegisterButton }

procedure TuRegisterButton.Click;
begin
  inherited;
  if _Application<>nil then
    _Application.RegisterID;
end;

{ TuChangePassButton }

procedure TuChangePassButton.Click;
begin
  inherited;
  if _Application<>nil then
    _Application.ChangePassWord;
end;

//procedure TuFplCombobox.Change;
//begin
//  if _Application<>nil then
//    _Application.FPLOnChange;
//end;

{ TuLable }

constructor TuLable.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize    :=  False;
  TransParent :=  True;
end;

procedure TuLable.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ReleaseCapture;
    TControl(Owner).Perform(WM_SysCommand, $F012, 0);
  end;
end;

{ TuInfLable }

constructor TuInfLable.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize    :=  False;
  TransParent :=  True;
end;

procedure TuInfLable.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ReleaseCapture;
    TControl(Owner).Perform(WM_SysCommand, $F012, 0);
  end;
end;

{ TuImage }

procedure TuImage.Loaded;
begin
  inherited;
  if (Picture.Graphic<>nil) and (Picture.Graphic is TGIFImage) then
    TGIFImage(Picture.Graphic).Animate  :=  True;
end;

procedure TuImage.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ReleaseCapture;
    TControl(Owner).Perform(WM_SysCommand, $F012, 0);
  end;
end;

{ TuTreeView }

constructor TuTreeView.Create(AOwner: TComponent);
begin
  inherited;
  ReadOnly    :=  True;
  HideSelection := False;
  ShowRoot := False;
  BevelInner := bvNone;
  BevelKind := bkNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
end;

{ TuServerCombobox }

constructor TuServerCombobox.Create(AOwner: TComponent);
begin
  inherited;
 // Style := csExDropDownList;
  Style := csOwnerDrawVariable;



end;
constructor TuFplCombobox.Create(AOwner: TComponent);
begin
  inherited;
  Style := csDropDownList;
 /// Style := csOwnerDrawVariable;



end;



{ TEmbeddedFiles }

function TEmbeddedFiles.Add: TEmbeddedFileItem;
begin
  Result := TEmbeddedFileItem(inherited Add);
end;

function TEmbeddedFiles.Get(index: Integer): TEmbeddedFileItem;
begin
  Result := TEmbeddedFileItem(inherited Items[index]);
end;

{ TuCustomForm }

constructor TuCustomForm.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner, 0);
    FEmbeddedFiles := TEmbeddedFiles.Create(TEmbeddedFileItem);
    if (ClassType <> TuCustomForm) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        InitInheritedComponent(Self, TuCustomForm);
      finally
        Exclude(FFormState, fsCreating);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

destructor TuCustomForm.Destroy;
begin
  FreeAndNil(FEmbeddedFiles);
  inherited;
end;

procedure TuCustomForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ReleaseCapture;
    Perform(WM_SysCommand, $F012, 0);
  end;
end;

procedure TuCustomForm.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if Assigned(FWindowPosChanged) then
    FWindowPosChanged(Self);
end;

{ TuWebBrowser }

constructor TuWebBrowser.Create(Owner: TComponent);
begin
  inherited;
  UserInterfaceOptions := UserInterfaceOptions + [DontUse3DBorders];
end;

{ TuGetBackPassButton }

procedure TuGetBackPassButton.Click;
begin
  inherited;
  if _Application<>nil then
    _Application.GetBackPassWord;
end;

initialization
  RegisterClasses([
    TuImageButton,
    TuURLButton,
    TuStartBtton,
    TuMinimizeButton,
    TuCloseButton,
    TuConfigButton,
    TuRegisterButton,
    TuChangePassButton,
    TuGetBackPassButton,
    TuWebBrowser,
    TuCurProgressBar,
    TuAllProgressBar,
    TuLable,
    TuInfLable,
    TuImage,
    TuTreeView,
    TuServerCombobox,
    TuFplCombobox,
    TuForm
  ]);

end.
