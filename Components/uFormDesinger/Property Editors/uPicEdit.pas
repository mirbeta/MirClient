unit uPicEdit;

interface

uses Windows, Classes, Graphics, Forms, Controls, Dialogs, Buttons, uDesignIntf,
  uDesignEditors, StdCtrls, ExtCtrls, ExtDlgs;

type
  TPictureEditorDlg = class(TForm)
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    GroupBox1: TGroupBox;
    ImagePanel: TPanel;
    Load: TButton;
    Save: TButton;
    Clear: TButton;
    ImagePaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ImagePaintBoxPaint(Sender: TObject);
  private
    Pic: TPicture;
  end;

  TPictureEditor = class(TComponent)
  private
    FGraphicClass: TGraphicClass;
    FPicture: TPicture;
    FPicDlg: TPictureEditorDlg;
    procedure SetPicture(Value: TPicture);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property GraphicClass: TGraphicClass read FGraphicClass write FGraphicClass;
    property Picture: TPicture read FPicture write SetPicture;
  end;

{ TPictureProperty
  Property editor the TPicture properties (e.g. the Picture property).  Brings
  up a file open dialog allowing loading a picture file. }

  TPictureProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TGraphicProperty }

  TGraphicProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TGraphicEditor }

  TGraphicEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  end;

implementation

uses TypInfo, SysUtils, uDesignConst;

{$R *.dfm}

{ TPictureEditorDlg }

procedure TPictureEditorDlg.FormCreate(Sender: TObject);
begin
//  HelpContext := hcDPictureEditor;
  Pic := TPicture.Create;
  Save.Enabled := False;
end;

procedure TPictureEditorDlg.FormDestroy(Sender: TObject);
begin
  Pic.Free;
end;

procedure TPictureEditorDlg.LoadClick(Sender: TObject);
begin
  OpenDialog.Title := SLoadPictureTitle;
  if OpenDialog.Execute then
  begin
    Pic.LoadFromFile(OpenDialog.Filename);
    ImagePaintBox.Invalidate;
    Save.Enabled := (Pic.Graphic <> nil) and not Pic.Graphic.Empty;
    Clear.Enabled := (Pic.Graphic <> nil) and not Pic.Graphic.Empty;
  end;
end;

procedure TPictureEditorDlg.SaveClick(Sender: TObject);
begin
  if Pic.Graphic <> nil then
  begin
    SaveDialog.Title := SSavePictureTitle;
    with SaveDialog do
    begin
      DefaultExt := GraphicExtension(TGraphicClass(Pic.Graphic.ClassType));
      Filter := GraphicFilter(TGraphicClass(Pic.Graphic.ClassType));
      if Execute then Pic.SaveToFile(Filename);
    end;
  end;
end;

procedure TPictureEditorDlg.ImagePaintBoxPaint(Sender: TObject);
var
  DrawRect: TRect;
  SNone: string;
begin
  with TPaintBox(Sender) do
  begin
    Canvas.Brush.Color := {Self.}Color;
    DrawRect := ClientRect;//Rect(Left, Top, Left + Width, Top + Height);
    if Pic.Width > 0 then
    begin
      with DrawRect do
        if (Pic.Width > Right - Left) or (Pic.Height > Bottom - Top) then
        begin
          if Pic.Width > Pic.Height then
            Bottom := Top + MulDiv(Pic.Height, Right - Left, Pic.Width)
          else
            Right := Left + MulDiv(Pic.Width, Bottom - Top, Pic.Height);
          Canvas.StretchDraw(DrawRect, Pic.Graphic);
        end
        else
          with DrawRect do
            Canvas.Draw(Left + (Right - Left - Pic.Width) div 2, Top + (Bottom - Top -
              Pic.Height) div 2, Pic.Graphic);
    end
    else
      with DrawRect, Canvas do
      begin
        SNone := srNone;
        TextOut(Left + (Right - Left - TextWidth(SNone)) div 2, Top + (Bottom -
          Top - TextHeight(SNone)) div 2, SNone);
      end;
  end;
end;

procedure TPictureEditorDlg.ClearClick(Sender: TObject);
begin
  Pic.Graphic := nil;
  ImagePaintBox.Invalidate;
  Save.Enabled := False;
  Clear.Enabled := False;
end;

{ TPictureEditor }

constructor TPictureEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicDlg := TPictureEditorDlg.Create(Self);
  FGraphicClass := TGraphic;
end;

destructor TPictureEditor.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TPictureEditor.Execute: Boolean;
begin
  FPicDlg.Pic.Assign(FPicture);
  with FPicDlg.OpenDialog do
  begin
    Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp];
    DefaultExt := GraphicExtension(GraphicClass);
    Filter := GraphicFilter(GraphicClass);
//    HelpContext := hcDLoadPicture;
  end;
  with FPicDlg.SaveDialog do
  begin
    Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp];
    DefaultExt := GraphicExtension(GraphicClass);
    Filter := GraphicFilter(GraphicClass);
//    HelpContext := hcDSavePicture;
  end;
  FPicDlg.Save.Enabled := (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty;
  FPicDlg.Clear.Enabled := (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty;
  Result := FPicDlg.ShowModal = mrOK;
  if Result then FPicture.Assign(FPicDlg.Pic);
end;

procedure TPictureEditor.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TPictureProperty }

procedure TPictureProperty.Edit;
var
  PictureEditor: TPictureEditor;
begin
  PictureEditor := TPictureEditor.Create(nil);
  try
    PictureEditor.Picture := TPicture(Pointer(GetOrdValue));
    if PictureEditor.Execute then
      SetOrdValue(Longint(PictureEditor.Picture));
  finally
    PictureEditor.Free;
  end;
end;

function TPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TPictureProperty.GetValue: string;
var
  Picture: TPicture;
begin
  Picture := TPicture(GetOrdValue);
  if Picture.Graphic = nil then
    Result := srNone else
    Result := '(' + Picture.Graphic.ClassName + ')';
end;

procedure TPictureProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TGraphicProperty }

procedure TGraphicProperty.Edit;
var
  PictureEditor: TPictureEditor;
begin
  PictureEditor := TPictureEditor.Create(nil);
  try
    PictureEditor.GraphicClass := TGraphicClass(GetTypeData(GetPropType)^.ClassType);
    PictureEditor.Picture.Graphic := TGraphic(Pointer(GetOrdValue));
    if PictureEditor.Execute then
    begin
      SetOrdValue(LongInt(PictureEditor.Picture.Graphic))
    end;
  finally
    PictureEditor.Free;
  end;
end;

function TGraphicProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGraphicProperty.GetValue: string;
var
  Graphic: TGraphic;
begin
  Graphic := TGraphic(GetOrdValue);
  if (Graphic = nil) or Graphic.Empty then
    Result := srNone else
    Result := '(' + Graphic.ClassName + ')';
end;

procedure TGraphicProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TPictureEditor }

procedure TGraphicEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'PICTURE') or
     SameText(PropName, 'IMAGE') then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

procedure TPictureEditorDlg.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
