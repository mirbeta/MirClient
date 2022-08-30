{*************************************************************************}
{ TAdvSmoothFillEditor editor                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2009                                              }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit AdvSmoothFillEditor;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GDIPFill, StdCtrls, AdvSmoothFillPreview, AdvSmoothSelectors,
  ExtCtrls, Mask, AdvSmoothSpin, ComCtrls, Buttons, Registry,
  AdvGDIP
  ;

type
  TAdvSmoothFillEditorForm = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    AdvSmoothColorSelector1: TAdvSmoothColorSelector;
    AdvSmoothColorSelector2: TAdvSmoothColorSelector;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    AdvSmoothColorSelector3: TAdvSmoothColorSelector;
    AdvSmoothColorSelector4: TAdvSmoothColorSelector;
    Label6: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    Label10: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ColorDialog1: TColorDialog;
    Label11: TLabel;
    ComboBox5: TComboBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    AdvSmoothSpinEdit5: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit6: TAdvSmoothSpinEdit;
    GroupBox2: TGroupBox;
    Label18: TLabel;
    Button5: TButton;
    Button6: TButton;
    ComboBox6: TComboBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    AdvSmoothSpinEdit7: TAdvSmoothSpinEdit;
    AdvSmoothSpinEdit8: TAdvSmoothSpinEdit;
    Label22: TLabel;
    ComboBox7: TComboBox;
    AdvSmoothSpinEdit9: TAdvSmoothSpinEdit;
    Label23: TLabel;
    Label24: TLabel;
    AdvSmoothSpinEdit10: TAdvSmoothSpinEdit;
    GroupBox3: TGroupBox;
    Label25: TLabel;
    AdvSmoothColorSelector5: TAdvSmoothColorSelector;
    Label26: TLabel;
    AdvSmoothSpinEdit11: TAdvSmoothSpinEdit;
    Label27: TLabel;
    ComboBox8: TComboBox;
    Label28: TLabel;
    AdvSmoothSpinEdit12: TAdvSmoothSpinEdit;
    GroupBox4: TGroupBox;
    Label29: TLabel;
    Label30: TLabel;
    AdvSmoothColorSelector6: TAdvSmoothColorSelector;
    AdvSmoothSpinEdit13: TAdvSmoothSpinEdit;
    Label31: TLabel;
    AdvSmoothSpinEdit14: TAdvSmoothSpinEdit;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    TrackBar5: TTrackBar;
    Label36: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ComboBox9: TComboBox;
    Label37: TLabel;
    GroupBox6: TGroupBox;
    Label38: TLabel;
    AdvSmoothColorSelector7: TAdvSmoothColorSelector;
    Label39: TLabel;
    AdvSmoothColorSelector8: TAdvSmoothColorSelector;
    Label40: TLabel;
    ComboBox10: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox11: TComboBox;
    Label41: TLabel;
    ComboBox12: TComboBox;
    Label42: TLabel;
    Label43: TLabel;
    ComboBox13: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AdvSmoothColorSelector1Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothColorSelector3Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothColorSelector2Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothColorSelector4Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothSpinEdit2Change(Sender: TObject);
    procedure AdvSmoothSpinEdit3Change(Sender: TObject);
    procedure AdvSmoothSpinEdit4Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure AdvSmoothSpinEdit5Change(Sender: TObject);
    procedure AdvSmoothSpinEdit6Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure AdvSmoothSpinEdit7Change(Sender: TObject);
    procedure AdvSmoothSpinEdit8Change(Sender: TObject);
    procedure AdvSmoothSpinEdit9Change(Sender: TObject);
    procedure AdvSmoothSpinEdit10Change(Sender: TObject);
    procedure AdvSmoothColorSelector5Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothSpinEdit11Change(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure AdvSmoothSpinEdit12Change(Sender: TObject);
    procedure AdvSmoothColorSelector6Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothSpinEdit13Change(Sender: TObject);
    procedure AdvSmoothSpinEdit14Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ComboBox9Change(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure AdvSmoothColorSelector7Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure AdvSmoothColorSelector8Select(Sender: TObject; Index: Integer;
      Item: TAdvSmoothSelectorItem);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox11Change(Sender: TObject);
    procedure ComboBox12Change(Sender: TObject);
    procedure ComboBox13Change(Sender: TObject);
  private
    FFillPreview: TGDIPFill;
    AdvSmoothFillPreview1, AdvSmoothFillPreview2, AdvSmoothFillPreview3: TAdvSmoothFillPreview;
  protected
    procedure FillChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    procedure Init(Create: Boolean);
    procedure SaveChanges;
  published
    property FillPreview: TGDIPFill read FFillPreview write FFillPreview;
  end;

 {$IFNDEF TMSDOTNET}
  TAdvSmoothFillEditorDialog = class(TCommonDialog)
  {$ENDIF}
  {$IFDEF TMSDOTNET}
  TAdvSmoothFillEditorDialog = class(TComponent)
  {$ENDIF}
  private
    FForm: TAdvSmoothFillEditorForm;
    FCaption: string;
    FFill: TGDIPFill;
    procedure SetFill(const Value: TGDIPFill);
  protected
  public
    {$IFNDEF TMSDOTNET}
    function Execute: Boolean; override;
    {$ENDIF}
    {$IFDEF TMSDOTNET}
    function Execute: Boolean;
    {$ENDIF}
    property Form: TAdvSmoothFillEditorForm read FForm;
    property Fill: TGDIPFill read FFill write SetFill;    
  published
    property Caption: string read FCaption write FCaption;
  end;

  TSaveMode = (mCancelled, mSaved, mNotSaved);

var
  AdvSmoothFillEditorForm: TAdvSmoothFillEditorForm;
  Mode: TSaveMode;
  f: TGDIPFill;

implementation

{$R *.dfm}

{ TFillEditorForm }

function GetBDSKey: string;
begin
  Result := '';
  {$IFDEF VER140}
  Result := 'Software\Borland\Delphi\6.0\Custom Colors';
  {$ENDIF}
  {$IFDEF VER150}
  Result := 'Software\Borland\Delphi\7.0\Custom Colors';
  {$ENDIF}
  {$IFDEF VER170}
  Result := 'Software\Borland\BDS\3.0\Custom Colors';
  {$ENDIF}
  {$IFDEF VER180}
  Result := 'Software\Borland\BDS\4.0\Custom Colors';
  {$ENDIF}
  {$IFDEF VER185}
  Result := 'Software\Borland\BDS\5.0\Custom Colors';
  {$ENDIF}
  {$IFDEF VER200}
  Result := 'Software\CodeGear\BDS\6.0\Custom Colors';
  {$ENDIF}
end;

procedure LoadCustomColors(ColorDialog: TColorDialog);
var
  reg: TRegistry;
  v: string;
  i: integer;
  key: string;
begin
  if GetBDSKey = '' then
    Exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(GetBDSKey,false) then
    begin
      ColorDialog.CustomColors.Clear;
      for i := 0 to 15 do
      begin
        key := 'Color'+ chr(ord('A')+i);
        v := reg.ReadString(key);
        ColorDialog.CustomColors.Add(key+'='+v);
      end;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure SaveCustomColors(ColorDialog: TColorDialog); var
  reg: tregistry;
  v: string;
  i,j: integer;
  key: string;
  HasCustomColors: boolean;
begin
  if GetBDSKey = '' then
    Exit;

  HasCustomColors := false;

  for i := 0 to ColorDialog.CustomColors.Count - 1 do
  begin
    if (pos('FFFFFFFF',ColorDialog.CustomColors[i]) = 0) then
      HasCustomColors := true;
  end;

  if not HasCustomColors then
    Exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(GetBDSKey,false) then
    begin
      for i := 0 to ColorDialog.CustomColors.Count - 1 do
      begin
        j := Pos('=',ColorDialog.CustomColors[i]);
        if (j >  0) then
        begin
          key := Copy(ColorDialog.CustomColors[i], 1, j - 1);
          v := Copy(ColorDialog.CustomColors[i], j + 1, length(ColorDialog.CustomColors[i]));
          reg.WriteString(key, v);
        end;
      end;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;


procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector1Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector1.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector1.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector1.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.Color := AdvSmoothColorSelector1.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector2Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector2.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector2.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector2.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.ColorTo := AdvSmoothColorSelector2.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector3Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector3.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector3.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector3.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.ColorMirror := AdvSmoothColorSelector3.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector4Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector4.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector4.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector4.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.ColorMirrorTo := AdvSmoothColorSelector4.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector5Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector5.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector5.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector5.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.BorderColor := AdvSmoothColorSelector5.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector6Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector6.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector6.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector6.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.ShadowColor := AdvSmoothColorSelector6.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector7Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector7.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector7.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector7.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.GlowGradientColor := AdvSmoothColorSelector7.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothColorSelector8Select(
  Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem);
begin
  if Index = 0 then
    AdvSmoothColorSelector8.SelectedColor := clNone;

  if Index = 41 then
  begin
    ColorDialog1.Color := AdvSmoothColorSelector8.SelectedColor;
    if ColorDialog1.Execute then
    begin
      AdvSmoothColorSelector8.SelectedColor := ColorDialog1.Color;
    end;
  end;

  f.GlowRadialColor := AdvSmoothColorSelector8.SelectedColor;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit10Change(Sender: TObject);
begin
  f.PictureHeight := AdvSmoothSpinEdit10.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit11Change(Sender: TObject);
begin
  f.BorderWidth := AdvSmoothSpinEdit11.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit12Change(Sender: TObject);
begin
  f.Rounding := AdvSmoothSpinEdit12.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit13Change(Sender: TObject);
begin
  f.ShadowOffset := AdvSmoothSpinEdit13.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit14Change(Sender: TObject);
begin
  f.Angle := AdvSmoothSpinEdit14.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit2Change(Sender: TObject);
begin
  f.OpacityTo := TrackBar2.Position;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit3Change(Sender: TObject);
begin
  f.OpacityMirror := TrackBar3.Position;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit4Change(Sender: TObject);
begin
  f.OpacityMirrorTo := TrackBar4.Position;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit5Change(Sender: TObject);
begin
  f.BackGroundPictureLeft := AdvSmoothSpinEdit5.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit6Change(Sender: TObject);
begin
  f.BackGroundPicturetop := AdvSmoothSpinEdit6.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit7Change(Sender: TObject);
begin
  f.PictureLeft := AdvSmoothSpinEdit7.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit8Change(Sender: TObject);
begin
  f.PictureTop := AdvSmoothSpinEdit8.Value;
end;

procedure TAdvSmoothFillEditorForm.AdvSmoothSpinEdit9Change(Sender: TObject);
begin
  f.PictureWidth := AdvSmoothSpinEdit9.Value;
end;

procedure TAdvSmoothFillEditorForm.Button1Click(Sender: TObject);
begin
  Mode := mSaved;
  SaveChanges;
  Self.Close;
end;

procedure TAdvSmoothFillEditorForm.Button2Click(Sender: TObject);
begin
  f.BackGroundPicture := nil;
  AdvSmoothFillPreview2.Fill.BackGroundPicture := nil;
end;

procedure TAdvSmoothFillEditorForm.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    f.BackGroundPicture.LoadFromFile(OpenDialog1.FileName);
    AdvSmoothFillPreview2.Fill.BackGroundPicture.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TAdvSmoothFillEditorForm.Button4Click(Sender: TObject);
begin
  Mode := mCancelled;
  Self.Close;
end;

procedure TAdvSmoothFillEditorForm.Button5Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    f.Picture.LoadFromFile(OpenDialog1.FileName);
    AdvSmoothFillPreview3.Fill.BackGroundPicture.LoadFromFile(OpenDialog1.FileName);    
  end;
end;

procedure TAdvSmoothFillEditorForm.Button6Click(Sender: TObject);
begin
  f.Picture := nil;
  AdvSmoothFillPreview3.Fill.BackGroundPicture := nil;
end;

procedure TAdvSmoothFillEditorForm.CheckBox1Click(Sender: TObject);
begin
  f.BackGroundPictureAspectRatio := CheckBox1.Checked;
end;

procedure TAdvSmoothFillEditorForm.CheckBox2Click(Sender: TObject);
begin
  f.PictureAspectRatio := CheckBox2.Checked;
end;

procedure TAdvSmoothFillEditorForm.ComboBox10Change(Sender: TObject);
begin
  f.Glow := TGlowMode(ComboBox10.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox11Change(Sender: TObject);
begin
  f.BackGroundPictureAspectMode := TPictureMode(ComboBox11.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox12Change(Sender: TObject);
begin
  f.PictureAspectMode := TPictureMode(ComboBox12.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox13Change(Sender: TObject);
begin
  f.ShadowType := TShadowType(ComboBox13.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox1Change(Sender: TObject);
var
  gt, resgt: TAdvGradientType;
begin
  gt := TAdvGradientType(Combobox1.ItemIndex);

  if gt = gtPath then
    resgt := gtTexture
  else if gt = gtTexture then
    resgt := gtNone
  else
    resgt := gt;

  f.GradientType := resgt;
end;

procedure TAdvSmoothFillEditorForm.ComboBox2Change(Sender: TObject);
var
  gt, resgt: TAdvGradientType;
begin
  gt := TAdvGradientType(Combobox2.ItemIndex);

  if gt = gtPath then
    resgt := gtTexture
  else if gt = gtTexture then
    resgt := gtNone
  else
    resgt := gt;

  f.GradientMirrorType := resgt;
end;

procedure TAdvSmoothFillEditorForm.ComboBox3Change(Sender: TObject);
begin
  f.HatchStyle := THatchStyle(ComboBox3.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox4Change(Sender: TObject);
begin
  f.HatchStyleMirror := THatchStyle(ComboBox4.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox5Change(Sender: TObject);
begin
  f.BackGroundPicturePosition := TFillPicturePosition(ComboBox5.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox6Change(Sender: TObject);
begin
  f.PicturePosition := TFillPicturePosition(ComboBox6.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox7Change(Sender: TObject);
begin
  f.PictureSize := TFillPictureSize(ComboBox7.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox8Change(Sender: TObject);
begin
  f.RoundingType := TFillRoundingType(ComboBox8.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.ComboBox9Change(Sender: TObject);
begin
  f.BackGroundPictureMode := TFIllPictureMode(ComboBox9.ItemIndex);
end;

procedure TAdvSmoothFillEditorForm.FillChanged(Sender: TObject);
begin
  AdvSmoothFillPreview1.Fill.Assign(f);
  AdvSmoothFillPreview1.Invalidate;
end;

procedure TAdvSmoothFillEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveCustomColors(ColorDialog1);
  if Mode = mNotSaved then
  begin
    case MessageDlg('Save Changes ?',mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        SaveChanges;
        f.Free;
      end;
      mrNo:
      begin
        f.Free;
      end;
      mrCancel: Action := caNone;
    end;
  end
  else
  begin
    f.Free;
  end;
end;

procedure TAdvSmoothFillEditorForm.Init(Create: Boolean);
begin
  if not Assigned(AdvSmoothFillPreview1) then
  begin
    AdvSmoothFillPreview1 := TAdvSmoothFillPreview.Create(GroupBox5);
    AdvSmoothFillPreview1.Parent := GroupBox5;
    AdvSmoothFillPreview1.Left := 19;
    AdvSmoothFillPreview1.Top := 22;
    AdvSmoothFillPreview1.Width := 212;
    AdvSmoothFillPreview1.Height := 181;
  end;

  if not Assigned(AdvSmoothFillPreview2) then
  begin
    AdvSmoothFillPreview2 := TAdvSmoothFillPreview.Create(GroupBox1);
    AdvSmoothFillPreview2.Parent := GroupBox1;
    AdvSmoothFillPreview2.Left := 182;
    AdvSmoothFillPreview2.Top := 267;
    AdvSmoothFillPreview2.Width := 50;
    AdvSmoothFillPreview2.Height := 50;
    AdvSmoothFillPreview2.Fill.GradientType := gtTexture;
    AdvSmoothFillPreview2.Fill.BackGroundPicturePosition := ppStretched;
  end;

  if not Assigned(AdvSmoothFillPreview3) then
  begin
    AdvSmoothFillPreview3 := TAdvSmoothFillPreview.Create(GroupBox2);
    AdvSmoothFillPreview3.Parent := GroupBox2;
    AdvSmoothFillPreview3.Left := 384;
    AdvSmoothFillPreview3.Top := 18;
    AdvSmoothFillPreview3.Width := 50;
    AdvSmoothFillPreview3.Height := 50;
    AdvSmoothFillPreview3.Fill.GradientType := gtTexture;
    AdvSmoothFillPreview3.Fill.BackGroundPicturePosition := ppStretched;
  end;

  Mode := mNotSaved;
  if Create then
  begin
    f := TGDIPFill.Create;
    f.OnChange := FillChanged;
    f.Assign(FillPreview);
  end;

  AdvSmoothFillPreview1.Fill.Assign(f);

  AdvSmoothColorSelector1.SelectedColor := f.Color;
  AdvSmoothColorSelector2.SelectedColor := f.ColorTo;
  AdvSmoothColorSelector3.SelectedColor := f.ColorMirror;
  AdvSmoothColorSelector4.SelectedColor := f.ColorMirrorTo;

  TrackBar1.Position := f.Opacity;
  Label35.Caption := IntToStr(Round((F.Opacity / 255) * 100)) + '%';
  TrackBar2.Position := f.OpacityTo;
  Label34.Caption := IntToStr(Round((F.OpacityTo / 255) * 100)) + '%';
  TrackBar3.Position := f.OpacityMirror;
  Label33.Caption := IntToStr(Round((F.OpacityMirror / 255) * 100)) + '%';
  TrackBar4.Position := f.OpacityMirrorTo;
  Label32.Caption := IntToStr(Round((F.OpacityMirrorTo / 255) * 100)) + '%';

  TrackBar5.Position := f.BorderOpacity;
  Label36.Caption := IntToStr(Round((F.BorderOpacity / 255) * 100)) + '%';

  case f.GradientType of
    gtTexture: ComboBox1.ItemIndex := 8;
    gtNone: ComboBox1.ItemIndex := 9;
    else
      ComboBox1.ItemIndex := Integer(f.GradientType);
  end;

  case f.GradientMirrorType of
    gtTexture: ComboBox2.ItemIndex := 8;
    gtNone: ComboBox2.ItemIndex := 9;
    else
      ComboBox2.ItemIndex := Integer(f.GradientMirrorType);
  end;

  ComboBox3.ItemIndex := Integer(f.HatchStyle);
  ComboBox4.ItemIndex := Integer(f.HatchStyleMirror);

  //Pictures ??? todo
  AdvSmoothFillPreview2.Fill.BackGroundPicture.Assign(f.BackGroundPicture);
  AdvSmoothFillPreview3.Fill.BackGroundPicture.Assign(f.Picture);  
  ComboBox5.ItemIndex := Integer(F.BackGroundPicturePosition);
  AdvSmoothSpinEdit5.Value := f.BackGroundPictureLeft;
  AdvSmoothSpinEdit6.Value := f.BackGroundPictureTop;
  ComboBox6.ItemIndex := Integer(f.PicturePosition);
  AdvSmoothSpinEdit7.Value := f.PictureLeft;
  AdvSmoothSpinEdit8.Value := f.PictureTop;
  ComboBox7.ItemIndex := Integer(f.PictureSize);
  AdvSmoothSpinEdit9.Value := f.PictureWidth;
  AdvSmoothSpinEdit10.Value := f.PictureHeight;

  AdvSmoothColorSelector5.SelectedColor := f.BorderColor;
  AdvSmoothSpinEdit11.Value := f.BorderWidth;
  ComboBox8.ItemIndex := Integer(f.RoundingType);
  AdvSmoothSpinEdit12.Value := f.Rounding;

  ComboBox11.ItemIndex := Integer(f.BackGroundPictureAspectMode);
  ComboBox12.ItemIndex := Integer(f.PictureAspectMode);
  combobox13.ItemIndex := Integer(f.ShadowType);

  AdvSmoothColorSelector6.SelectedColor := f.ShadowColor;
  AdvSmoothSpinEdit13.Value := f.ShadowOffset;

  AdvSmoothSpinEdit14.Value := f.Angle;

  ComboBox9.ItemIndex := Integer(f.BackGroundPictureMode);
  ComboBox10.ItemIndex := Integer(f.Glow);
  AdvSmoothColorSelector7.SelectedColor := f.GlowGradientColor;
  AdvSmoothColorSelector8.SelectedColor := f.GlowRadialColor;

  CheckBox1.Checked := f.BackGroundPictureAspectRatio;
  CheckBox2.Checked := f.PictureAspectRatio;

  LoadCustomColors(ColorDialog1);
end;

procedure TAdvSmoothFillEditorForm.SaveChanges;
begin
  FFillPreview.Assign(f);
end;

procedure TAdvSmoothFillEditorForm.SpeedButton1Click(Sender: TObject);
begin
  f.LoadFromClipBoard;
  Init(false);
end;

procedure TAdvSmoothFillEditorForm.SpeedButton2Click(Sender: TObject);
begin
  f.SaveToClipBoard;
end;

procedure TAdvSmoothFillEditorForm.TrackBar1Change(Sender: TObject);
begin
  f.Opacity := TrackBar1.Position;
  Label35.Caption := IntToStr(Round((F.Opacity / 255) * 100)) + '%';
end;

procedure TAdvSmoothFillEditorForm.TrackBar2Change(Sender: TObject);
begin
  f.OpacityTo := TrackBar2.Position;
  Label34.Caption := IntToStr(Round((F.OpacityTo / 255) * 100)) + '%';
end;

procedure TAdvSmoothFillEditorForm.TrackBar3Change(Sender: TObject);
begin
  f.OpacityMirror := TrackBar3.Position;
  Label33.Caption := IntToStr(Round((F.OpacityMirror / 255) * 100)) + '%';
end;

procedure TAdvSmoothFillEditorForm.TrackBar4Change(Sender: TObject);
begin
  f.OpacityMirrorTo := TrackBar4.Position;
  Label32.Caption := IntToStr(Round((F.OpacityMirrorTo / 255) * 100)) + '%';
end;

procedure TAdvSmoothFillEditorForm.TrackBar5Change(Sender: TObject);
begin
  f.BorderOpacity := TrackBar5.Position;
  Label36.Caption := IntToStr(Round((F.BorderOpacity / 255) * 100)) + '%';  
end;

{ TAdvSmoothFillEditorDialog }

function TAdvSmoothFillEditorDialog.Execute: Boolean;
begin
  FForm := TAdvSmoothFillEditorForm.Create(Application);

  if not Assigned(FFill) then
  begin
    raise Exception.Create('The dialog does not have a TGDIPFill class assigned.');
    Result := False;
    Exit;
  end;

  if FCaption <> '' then
    Form.Caption := FCaption;

  try
    FForm.Fillpreview := FFill;
    FForm.Init(true);
    Result := FForm.ShowModal = mrOK;
  finally
    FForm.Free;
  end;
end;

procedure TAdvSmoothFillEditorDialog.SetFill(const Value: TGDIPFill);
begin
  FFill := Value;
end;

end.
