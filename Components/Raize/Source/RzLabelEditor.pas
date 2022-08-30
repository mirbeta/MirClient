{===============================================================================
  RzLabelEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzLableEditor
    Adds context menu and advanced editing dialog.


  Modification History
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Enhanced the TRzLabelEditor so that the developer is able to enter a
      multi-line caption along with the other properties accessible in the
      component editor.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes. Also uses the TRzPageControl.
===============================================================================}

{$I RzComps.inc}

unit RzLabelEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Menus,
  Forms,
  Dialogs,
  StdCtrls,
  Rzlabel,
  ExtCtrls,
  Buttons,
  RzTrkBar,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzDesignEditors,
  RzCmboBx,
  Tabs,
  RzPanel,
  RzRadGrp,
  RzTabs,
  RzRadChk,
  Mask,
  RzEdit,
  RzButton,
  RzCommon;

type
  TRzLabelEditor = class( TRzComponentEditor )
  protected
    function LabelControl: TRzLabel;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure TextStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzLabelEditDlg = class(TForm)
    grpPreview: TRzGroupBox;
    lblPreview: TRzLabel;
    btnOK: TRzButton;
    btnCancel: TRzButton;
    Label1: TRzLabel;
    edtCaption: TRzMemo;
    pgcFormat: TRzPageControl;
    tabTextStyle: TRzTabSheet;
    tabOptions: TRzTabSheet;
    Label2: TRzLabel;
    Label3: TRzLabel;
    Label6: TRzLabel;
    trkPointSize: TRzTrackBar;
    grpFontStyle: TRzGroupBox;
    chkBold: TRzCheckBox;
    chkItalic: TRzCheckBox;
    chkStrikeout: TRzCheckBox;
    chkUnderline: TRzCheckBox;
    cbxFonts: TRzFontComboBox;
    grpTextStyle: TRzRadioGroup;
    grpShadow: TRzGroupBox;
    Label4: TRzLabel;
    Label5: TRzLabel;
    Label7: TRzLabel;
    LblShadowDepth: TRzLabel;
    trkShadow: TRzTrackBar;
    grpRotation: TRzGroupBox;
    lblAngle: TRzLabel;
    btnNone: TSpeedButton;
    btnFlat: TSpeedButton;
    btnCurve: TSpeedButton;
    trkAngle: TRzTrackBar;
    chk15Degrees: TRzCheckBox;
    optUpperLeft: TRzRadioButton;
    optUpperCenter: TRzRadioButton;
    optUpperRight: TRzRadioButton;
    optLeftCenter: TRzRadioButton;
    optCenter: TRzRadioButton;
    optRightCenter: TRzRadioButton;
    optLowerLeft: TRzRadioButton;
    optLowerCenter: TRzRadioButton;
    optLowerRight: TRzRadioButton;
    RzRegIniFile1: TRzRegIniFile;
    edtFontColor: TRzColorEdit;
    edtHighlightColor: TRzColorEdit;
    edtShadowColor: TRzColorEdit;
    RzCustomColors1: TRzCustomColors;
    chkLightStyle: TRzCheckBox;
    procedure edtCaptionChange(Sender: TObject);
    procedure grpTextStyleClick(Sender: TObject);
    procedure trkPointSizeDrawTick( TrackBar: TRzTrackBar; Canvas: TCanvas; Location: TPoint; Index: Integer );
    procedure trkPointSizeChange(Sender: TObject);
    procedure trkShadowChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxFontsChange(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkStrikeoutClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure trkAngleDrawTick(TrackBar: TRzTrackBar; Canvas: TCanvas; Location: TPoint; Index: Integer );
    procedure chk15DegreesClick(Sender: TObject);
    procedure trkAngleChange(Sender: TObject);
    procedure btnRotationClick(Sender: TObject);
    procedure optCenterPointClick(Sender: TObject);
    procedure chkLightStyleClick(Sender: TObject);
    procedure pgcFormatChanging(Sender: TObject; NewIndex: Integer;
      var AllowChange: Boolean);
    procedure edtFontColorChange(Sender: TObject);
    procedure edtHighlightColorChange(Sender: TObject);
    procedure edtShadowColorChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FUpdatingControls: Boolean;
    FNoAngleUpdate: Boolean;
  public
    procedure UpdateControls;
  end;


implementation

{$R *.dfm}

uses
  RzCaptionEditor;

{============================}
{== TRzLabelEditor Methods ==}
{============================}

function TRzLabelEditor.LabelControl: TRzLabel;
begin
  Result := Component as TRzLabel;
end;


function TRzLabelEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;


function TRzLabelEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Label...';
    1: Result := 'Align';
    2: Result := '-';
    3: Result := 'Optimize Size';
    4: Result := 'AutoSize';
    5: Result := 'WordWrap';
    6: Result := 'Transparent';
    7: Result := '-';
    8: Result := 'Text Style';
  end;
end;


function TRzLabelEditor.AlignMenuIndex: Integer;
begin
  Result := 1;
end;


procedure TRzLabelEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateTextStyleMenu( Style: TTextStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := LabelControl.TextStyle = Style;
    NewItem.OnClick := TextStyleMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    4: Item.Checked := LabelControl.AutoSize;
    5: Item.Checked := LabelControl.WordWrap;
    6: Item.Checked := LabelControl.Transparent;

    8:
    begin
      CreateTextStyleMenu( tsNormal, 'Normal' );
      CreateTextStyleMenu( tsRaised, 'Raised' );
      CreateTextStyleMenu( tsRecessed, 'Recessed' );
      CreateTextStyleMenu( tsShadow, 'Shadow' );
    end;
  end;
end;


procedure TRzLabelEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzLabelEditDlg;
  OwnerName, S: string;

  procedure CopyLabel( Dest, Source: TRzLabel );
  begin
    Dest.Caption := Source.Caption;
    Dest.Font := Source.Font;
    Dest.TextStyle := Source.TextStyle;
    Dest.LightTextStyle := Source.LightTextStyle;
    Dest.HighlightColor := Source.HighlightColor;
    Dest.ShadowColor := Source.ShadowColor;
    Dest.ShadowDepth := Source.ShadowDepth;
    Dest.Rotation := Source.Rotation;
    Dest.Angle := Source.Angle;
    Dest.CenterPoint := Source.CenterPoint;
  end;

begin
  case Index of
    0:                                                         { Edit Label... }
    begin
      Dlg := TRzLabelEditDlg.Create( Application );

      try
        { Copy Attributes to Dialog Box lblPreview Component }
        CopyLabel( Dlg.lblPreview, LabelControl );
        CaptionToStringList( LabelControl.Caption, Dlg.edtCaption.Lines );

        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;
        Dlg.UpdateControls;

        if Dlg.ShowModal = mrOK then
        begin
          CopyLabel( LabelControl, Dlg.lblPreview );
          StringListToCaption( Dlg.edtCaption.Lines, S );
          LabelControl.Caption := S;
          DesignerModified;
        end;
      finally
        Dlg.Free;
      end;
    end;

    3: // Optimize Size
    begin
      // If AutoSize is True, then turn it off and then back on
      // If AutoSize is False, then turn it on, and then off
      LabelControl.AutoSize := not LabelControl.AutoSize;
      LabelControl.AutoSize := not LabelControl.AutoSize;
      DesignerModified;
    end;

    4: // AutoSize
    begin
      LabelControl.AutoSize := not LabelControl.AutoSize;
      DesignerModified;
    end;

    5: // WordWrap
    begin
      LabelControl.WordWrap := not LabelControl.WordWrap;
      DesignerModified;
    end;

    6: // Transparent
    begin
      LabelControl.Transparent := not LabelControl.Transparent;
      DesignerModified;
    end;
  end; { case }
end; {= TRzLabelEditor.ExecuteVerb =}



procedure TRzLabelEditor.TextStyleMenuHandler( Sender: TObject );
begin
  LabelControl.TextStyle := TTextStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{=================================================}
{== Implementation Specific Types and Constants ==}
{=================================================}

const                                           { Support Point Size Track Bar }
  PointSizes: array[ 0..18 ] of string[ 2 ] =
    ( '6', '7', '8', '9', '10', '11', '12', '14', '16', '18', '20',
      '22', '24', '28', '32', '40', '48', '64', '72' );


{=============================}
{== TRzLabelEditDlg Methods ==}
{=============================}

{= NOTE:  All changes made through the control on this dialog box affect only =}
{=        the preview label (lblPreview).  Only if the OK button is pressed   =}
{=        are the changes reflected in the selected component.                =}


procedure TRzLabelEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  PgcFormat.ActivePage := TabTextStyle;
  FNoAngleUpdate := False;

  RzRegIniFile1.Path := RC_SettingsKey;

  RzCustomColors1.Load( 'Custom Colors' );
end;


procedure TRzLabelEditDlg.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  RzCustomColors1.Save( 'Custom Colors' );
end;


procedure TRzLabelEditDlg.UpdateControls;

  function PositionFromPointSize( P: Integer ): Integer;
  var
    I: Integer;
  begin
    I := 0;
    while ( I < 18 ) and ( StrToInt( string( PointSizes[ I ] ) ) <> P ) do
      Inc( I );
    if I = 18 then
      Result := 2
    else
      Result := I;
  end;

begin {= TRzLabelEditDlg.UpdateControls =}
  FUpdatingControls := True;
  try
    cbxFonts.SelectedFont := lblPreview.Font;
    edtFontColor.SelectedColor := lblPreview.Font.Color;
    trkPointSize.Position := PositionFromPointSize( lblPreview.Font.Size );

    { Font Styles }
    chkBold.Checked := fsBold in lblPreview.Font.Style;
    chkItalic.Checked := fsItalic in lblPreview.Font.Style;
    chkStrikeout.Checked := fsStrikeout in lblPreview.Font.Style;
    chkUnderline.Checked := fsUnderline in lblPreview.Font.Style;

    { Text Style }
    grpTextStyle.ItemIndex := Ord( lblPreview.TextStyle );
    chkLightStyle.Checked := lblPreview.LightTextStyle;

    { Shadow Options }
    edtHighlightColor.SelectedColor := lblPreview.HighlightColor;
    edtShadowColor.SelectedColor := lblPreview.ShadowColor;
    trkShadow.Position := lblPreview.ShadowDepth;

    { Rotation Options }
    trkAngle.Position := lblPreview.Angle;

    case lblPreview.Rotation of
      roNone: btnNone.Down := True;
      roFlat: btnFlat.Down := True;
      roCurve: btnCurve.Down := True;
    end;
    case lblPreview.CenterPoint of
      cpUpperLeft: optUpperLeft.Checked := True;
      cpUpperCenter: optUpperCenter.Checked := True;
      cpUpperRight: optUpperRight.Checked := True;
      cpLeftCenter: optLeftCenter.Checked := True;
      cpCenter: optCenter.Checked := True;
      cpRightCenter: optRightCenter.Checked := True;
      cpLowerLeft: optLowerLeft.Checked := True;
      cpLowerCenter: optLowerCenter.Checked := True;
      cpLowerRight: optLowerRight.Checked := True;
    end;
  finally
    FUpdatingControls := False;
  end;
end; {= TRzLabelEditDlg.UpdateControls =}


procedure TRzLabelEditDlg.edtCaptionChange(Sender: TObject);
begin
  lblPreview.Caption := edtCaption.Text;
end;


procedure TRzLabelEditDlg.cbxFontsChange(Sender: TObject);
begin
  lblPreview.Font.Name := cbxFonts.FontName;
end;

procedure TRzLabelEditDlg.chkLightStyleClick(Sender: TObject);
begin
  lblPreview.LightTextStyle := chkLightStyle.Checked;
end;



{= TRzLabelEditDlg.trkPointSizeDrawTick                                       =}
{=   Owner draw method is used to display point values at each tick mark.     =}

procedure TRzLabelEditDlg.trkPointSizeDrawTick(TrackBar: TRzTrackBar;
  Canvas: TCanvas; Location: TPoint; Index: Integer);
var
  W: Integer;
begin
  Canvas.Brush.Color := TrackBar.Color;
  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Size := 7;
  Canvas.Font.Style := [];
  W := Canvas.TextWidth( string( PointSizes[ Index ] ) );
  Canvas.TextOut( Location.X - (W div 2), 1, string( PointSizes[ Index ] ) );
end;


procedure TRzLabelEditDlg.trkPointSizeChange(Sender: TObject);
begin
  lblPreview.Font.Size := StrToInt( string( PointSizes[ trkPointSize.Position ] ) );
end;


procedure TRzLabelEditDlg.chkBoldClick(Sender: TObject);
begin
  if chkBold.Checked then
    lblPreview.Font.Style := lblPreview.Font.Style + [ fsBold ]
  else
    lblPreview.Font.Style := lblPreview.Font.Style - [ fsBold ]
end;


procedure TRzLabelEditDlg.chkItalicClick(Sender: TObject);
begin
  if chkItalic.Checked then
    lblPreview.Font.Style := lblPreview.Font.Style + [ fsItalic ]
  else
    lblPreview.Font.Style := lblPreview.Font.Style - [ fsItalic ]
end;


procedure TRzLabelEditDlg.chkStrikeoutClick(Sender: TObject);
begin
  if chkStrikeout.Checked then
    lblPreview.Font.Style := lblPreview.Font.Style + [ fsStrikeout ]
  else
    lblPreview.Font.Style := lblPreview.Font.Style - [ fsStrikeout ]
end;


procedure TRzLabelEditDlg.chkUnderlineClick(Sender: TObject);
begin
  if chkUnderline.Checked then
    lblPreview.Font.Style := lblPreview.Font.Style + [ fsUnderline ]
  else
    lblPreview.Font.Style := lblPreview.Font.Style - [ fsUnderline ]
end;


procedure TRzLabelEditDlg.grpTextStyleClick(Sender: TObject);
begin
  lblPreview.TextStyle := TTextStyle( grpTextStyle.ItemIndex );

  trkShadow.Enabled := lblPreview.TextStyle = tsShadow;
  lblShadowDepth.Enabled := lblPreview.TextStyle = tsShadow;
end;


procedure TRzLabelEditDlg.trkShadowChange(Sender: TObject);
begin
  lblPreview.ShadowDepth := trkShadow.Position;
  lblShadowDepth.Caption := IntToStr( trkShadow.Position );
end;


procedure TRzLabelEditDlg.edtFontColorChange(Sender: TObject);
begin
  lblPreview.Font.Color := edtFontColor.SelectedColor;
end;


procedure TRzLabelEditDlg.edtHighlightColorChange(Sender: TObject);
begin
  lblPreview.HighlightColor := edtHighlightColor.SelectedColor;
end;


procedure TRzLabelEditDlg.edtShadowColorChange(Sender: TObject);
begin
  lblPreview.ShadowColor := edtShadowColor.SelectedColor;
end;



procedure TRzLabelEditDlg.trkAngleDrawTick(TrackBar: TRzTrackBar;
  Canvas: TCanvas; Location: TPoint; Index: Integer);
var
  W, Degree: Integer;
begin
  if chk15Degrees.Checked then
    Degree := Index * 15
  else
    Degree := Index;

  if ( Degree mod 90 ) = 0 then
  begin
    Canvas.Brush.Color := TrackBar.Color;
    Canvas.Font.Name := 'Small Fonts';
    Canvas.Font.Size := 7;
    Canvas.Font.Style := [];
    W := Canvas.TextWidth( IntToStr( Degree ) );
    Canvas.TextOut( Location.X - (W div 2), 1, IntToStr( Degree ) );
  end;
end;


procedure TRzLabelEditDlg.chk15DegreesClick(Sender: TObject);
begin
  if chk15Degrees.Checked then
  begin
    trkAngle.Position := trkAngle.Position div 15;
    trkAngle.Max := 24;
  end
  else
  begin
    trkAngle.Max := 360;
    trkAngle.Position := trkAngle.Position * 15;
  end;
end;


procedure TRzLabelEditDlg.trkAngleChange(Sender: TObject);
var
  Angle: Integer;
begin
  if chk15Degrees.Checked then
    Angle := trkAngle.Position * 15
  else
    Angle := trkAngle.Position;
  LblAngle.Caption := IntToStr( Angle ) + '°';
  if not FNoAngleUpdate then
  begin
    if btnNone.Down and not FUpdatingControls then
    begin
      btnFlat.Down := True;
      btnRotationClick( btnFlat );
    end;
    lblPreview.Angle := Angle;
  end;
  cbxFonts.SelectedFont := lblPreview.Font;
end;


procedure TRzLabelEditDlg.btnRotationClick(Sender: TObject);
var
  Enable: Boolean;
begin
  with TSpeedButton( Sender ) do
  begin
    lblPreview.Rotation := TRotation( Tag );

    if lblPreview.Rotation = roNone then
    begin
      FNoAngleUpdate := True;
      try
        trkAngle.Position := 0;
      finally
        FNoAngleUpdate := False;
      end;
    end;

    if Tag <> 2 then
      optCenter.Checked := True;

    Enable := Tag = 2;
    optUpperLeft.Enabled := Enable;
    optUpperCenter.Enabled := Enable;
    optUpperRight.Enabled := Enable;
    optLeftCenter.Enabled := Enable;
    optCenter.Enabled := Enable;
    optRightCenter.Enabled := Enable;
    optLowerLeft.Enabled := Enable;
    optLowerCenter.Enabled := Enable;
    optLowerRight.Enabled := Enable;
  end;
end;


procedure TRzLabelEditDlg.optCenterPointClick(Sender: TObject);
begin
  lblPreview.CenterPoint := TCenterPoint( TRadioButton( Sender ).Tag );
end;


procedure TRzLabelEditDlg.pgcFormatChanging(Sender: TObject;
  NewIndex: Integer; var AllowChange: Boolean);
begin
  if NewIndex = 1 then
  begin
    if chk15Degrees.Checked then
      trkAngle.Position := lblPreview.Angle div 15
    else
      trkAngle.Position := lblPreview.Angle;
  end;
end;


end.
