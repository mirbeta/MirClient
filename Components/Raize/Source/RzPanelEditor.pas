{===============================================================================
  RzPanelEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editor
  ------------------------------------------------------------------------------
  TRzPanelEditor
    Adds context menu and custom dialog for changing properties.


  Modification History
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Added new "Add Button" menu item to designer context menu for TRzPanel.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added VisualStyle and GradientColorStyle menu items to designer context
      menu of TRzPanel.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added an "Add a Nested Panel" item to context menu.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
===============================================================================}

{$I RzComps.inc}

unit RzPanelEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Forms,
  Controls,
  Classes,
  Graphics,
  StdCtrls,
  Windows,
  ExtCtrls,
  Menus,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzDesignEditors,
  RzCmboBx,
  RzPanel,
  RzTrkBar,
  RzRadGrp,
  Mask,
  RzEdit,
  RzRadChk,
  RzButton,
  RzLabel,
  RzCommon;

type
  TRzPanelEditor = class( TRzComponentEditor )
  protected
    function Panel: TRzPanel;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzPanelEditDlg = class(TForm)
    GrpColors: TRzGroupBox;
    GrpWidth: TRzGroupBox;
    TrkWidth: TRzTrackBar;
    GrpPreview: TRzGroupBox;
    Label1: TRzLabel;
    GrpBorderStyle: TRzGroupBox;
    BtnOK: TRzButton;
    BtnCancel: TRzButton;
    PnlPreview: TRzPanel;
    Label2: TRzLabel;
    Label3: TRzLabel;
    OptBorderWidth: TRzRadioButton;
    OptBevelWidth: TRzRadioButton;
    LblOuter: TRzLabel;
    CbxOuter: TRzComboBox;
    LblInner: TRzLabel;
    CbxInner: TRzComboBox;
    Label4: TRzLabel;
    GrpCaption: TRzGroupBox;
    EdtCaption: TRzEdit;
    GrpAlignment: TRzRadioGroup;
    RzRegIniFile1: TRzRegIniFile;
    EdtPanelColor: TRzColorEdit;
    EdtBorderColor: TRzColorEdit;
    EdtBorderHighlight: TRzColorEdit;
    EdtBorderShadow: TRzColorEdit;
    RzCustomColors1: TRzCustomColors;
    procedure TrkWidthDrawTick(TrackBar: TRzTrackBar; Canvas: TCanvas;
      Location: TPoint; Index: Integer);
    procedure ChkBorderClick(Sender: TObject);
    procedure CbxOuterChange(Sender: TObject);
    procedure CbxInnerChange(Sender: TObject);
    procedure TrkWidthChange(Sender: TObject);
    procedure OptBorderWidthClick(Sender: TObject);
    procedure GrpAlignmentClick(Sender: TObject);
    procedure EdtCaptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdtPanelColorChange(Sender: TObject);
    procedure EdtBorderColorChange(Sender: TObject);
    procedure EdtBorderHighlightChange(Sender: TObject);
    procedure EdtBorderShadowChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure UpdateControls;
  end;




implementation

{$R *.dfm}

uses
  SysUtils,
  RzIntLst,
  RzStatus,
  RzPrgres,
  Buttons;


{============================}
{== TRzPanelEditor Methods ==}
{============================}

function TRzPanelEditor.Panel: TRzPanel;
begin
  Result := Component as TRzPanel;
end;


function TRzPanelEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;


function TRzPanelEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Panel...';
    1: Result := 'Align';
    2: Result := '-';
    3: Result := 'Visual Style';
    4: Result := 'Gradient Color Style';
    5: Result := 'Remove Border';
    6: Result := '-';
    7: Result := 'Add a Nested Panel';
    8: Result := 'Add Button';
  end;
end;



function TRzPanelEditor.AlignMenuIndex: Integer;
begin
  Result := 1;
end;


procedure TRzPanelEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: // VisualStyle
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, Panel.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, Panel.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, Panel.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    4: // GradientColorStyle
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, Panel.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, Panel.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, Panel.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;
  end;
end;


procedure TRzPanelEditor.ExecuteVerb( Index: Integer );
var
  Dialog: TRzPanelEditDlg;
  OwnerName: string;
  SubPanel: TRzPanel;

  procedure CopyPanel( Dest, Source: TRzPanel );
  begin
    Dest.Caption := Source.Caption;
    Dest.Alignment := Source.Alignment;
    Dest.AlignmentVertical := Source.AlignmentVertical;
    Dest.Color := Source.Color;
    Dest.BorderHighlight := Source.BorderHighlight;
    Dest.BevelWidth := Source.BevelWidth;
    Dest.BorderShadow := Source.BorderShadow;
    Dest.BorderOuter := Source.BorderOuter;
    Dest.BorderInner := Source.BorderInner;
    Dest.BorderWidth := Source.BorderWidth;
    Dest.BorderColor := Source.BorderColor;
    Dest.BorderSides := Source.BorderSides;
  end;

begin
  case Index of
    0: // Edit Panel
    begin
      Dialog := TRzPanelEditDlg.Create( Application );

      try
        { Copy Attributes to Dialog Box PnlPreview Component }
        CopyPanel( Dialog.PnlPreview, Panel );

        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dialog.Caption := OwnerName + Component.Name + Dialog.Caption;
        Dialog.UpdateControls;             { Update all controls on dialog box }

        if Dialog.ShowModal = mrOK then                   { Display Dialog Box }
        begin
          CopyPanel( Panel, Dialog.PnlPreview );
          DesignerModified;
        end;
      finally
        Dialog.Free;                         { Don't forget to free dialog box }
      end;
    end;

    5: // Remove Border
    begin
      Panel.BorderOuter := fsNone;
      Panel.BorderInner := fsNone;
      Panel.BorderWidth := 0;
      DesignerModified;
    end;

    7: // Add nested panel
    begin
      SubPanel := Designer.CreateComponent( TRzPanel, Panel, 10, 10, 50, 10 ) as TRzPanel;
      SubPanel.Align := alLeft;
      DesignerModified;
    end;

    8: // Add Button
    begin
      Designer.CreateComponent( TRzButton, Panel, 10, 10, 75, 25 );
      DesignerModified;
    end;
  end;
end; {= TRzPanelEditor.ExecuteVerb =}


procedure TRzPanelEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  Panel.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzPanelEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  Panel.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{=============================}
{== TRzPanelEditDlg Methods ==}
{=============================}

procedure TRzPanelEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  RzRegIniFile1.Path := RC_SettingsKey;
  RzCustomColors1.Load( 'Custom Colors' );
end;


procedure TRzPanelEditDlg.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  RzCustomColors1.Save( 'Custom Colors' );
end;


procedure TRzPanelEditDlg.UpdateControls;
begin
  EdtCaption.Text := PnlPreview.Caption;
  case PnlPreview.Alignment of
    taLeftJustify:
      GrpAlignment.ItemIndex := Ord( PnlPreview.AlignmentVertical );

    taCenter:
      GrpAlignment.ItemIndex := 3 + Ord( PnlPreview.AlignmentVertical );

    taRightJustify:
      GrpAlignment.ItemIndex := 6 + Ord( PnlPreview.AlignmentVertical );
  end;

  CbxOuter.ItemIndex := Ord( PnlPreview.BorderOuter );
  CbxInner.ItemIndex := Ord( PnlPreview.BorderInner );

  TrkWidth.Position := PnlPreview.BorderWidth;
  EdtBorderColor.SelectedColor := PnlPreview.BorderColor;
  EdtBorderHighlight.SelectedColor := PnlPreview.BorderHighlight;
  EdtBorderShadow.SelectedColor := PnlPreview.BorderShadow;
  EdtPanelColor.SelectedColor := PnlPreview.Color;
end;

procedure TRzPanelEditDlg.EdtCaptionChange(Sender: TObject);
begin
  PnlPreview.Caption := EdtCaption.Text;
end;

procedure TRzPanelEditDlg.TrkWidthDrawTick(TrackBar: TRzTrackBar;
  Canvas: TCanvas; Location: TPoint; Index: Integer);
var
  W: Integer;
  S: string;
begin
  if ( Index mod 5 = 0 ) or ( ( Index = 1 ) and OptBevelWidth.Checked ) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font.Name := 'Small Fonts';
    Canvas.Font.Size := 7;
    Canvas.Font.Style := [];
    S := IntToStr( Index );
    W := Canvas.TextWidth( S );
    Canvas.TextOut( Location.X - (W div 2), 1, S );
  end;
end;


procedure TRzPanelEditDlg.GrpAlignmentClick(Sender: TObject);
begin
  case GrpAlignment.ItemIndex of
    0:
    begin
      PnlPreview.Alignment := taLeftJustify;
      PnlPreview.AlignmentVertical := avTop;
    end;

    1:
    begin
      PnlPreview.Alignment := taLeftJustify;
      PnlPreview.AlignmentVertical := avCenter;
    end;

    2:
    begin
      PnlPreview.Alignment := taLeftJustify;
      PnlPreview.AlignmentVertical := avBottom;
    end;

    3:
    begin
      PnlPreview.Alignment := taCenter;
      PnlPreview.AlignmentVertical := avTop;
    end;

    4:
    begin
      PnlPreview.Alignment := taCenter;
      PnlPreview.AlignmentVertical := avCenter;
    end;

    5:
    begin
      PnlPreview.Alignment := taCenter;
      PnlPreview.AlignmentVertical := avBottom;
    end;

    6:
    begin
      PnlPreview.Alignment := taRightJustify;
      PnlPreview.AlignmentVertical := avTop;
    end;

    7:
    begin
      PnlPreview.Alignment := taRightJustify;
      PnlPreview.AlignmentVertical := avCenter;
    end;

    8:
    begin
      PnlPreview.Alignment := taRightJustify;
      PnlPreview.AlignmentVertical := avBottom;
    end;
  end;
end;


procedure TRzPanelEditDlg.ChkBorderClick(Sender: TObject);
begin
  if TCheckBox( Sender ).Checked then
    PnlPreview.BorderSides := PnlPreview.BorderSides + [ TSide( TCheckBox( Sender ).Tag ) ]
  else
    PnlPreview.BorderSides := PnlPreview.BorderSides - [ TSide( TCheckBox( Sender ).Tag ) ];
end;


procedure TRzPanelEditDlg.CbxOuterChange(Sender: TObject);
begin
  PnlPreview.BorderOuter := TFrameStyleEx( CbxOuter.ItemIndex );
end;


procedure TRzPanelEditDlg.CbxInnerChange(Sender: TObject);
begin
  PnlPreview.BorderInner := TFrameStyleEx( CbxInner.ItemIndex );
end;


procedure TRzPanelEditDlg.OptBorderWidthClick(Sender: TObject);
begin
  if Sender = OptBorderWidth then
  begin
    TrkWidth.Min := 0;
    TrkWidth.Position := PnlPreview.BorderWidth
  end
  else
  begin
    TrkWidth.Position := PnlPreview.BevelWidth;
    TrkWidth.Min := 1;
  end;
end;


procedure TRzPanelEditDlg.TrkWidthChange(Sender: TObject);
begin
  if OptBorderWidth.Checked then
    PnlPreview.BorderWidth := TrkWidth.Position
  else
    PnlPreview.BevelWidth := TrkWidth.Position;
end;


procedure TRzPanelEditDlg.EdtPanelColorChange(Sender: TObject);
begin
  PnlPreview.Color := EdtPanelColor.SelectedColor;
end;


procedure TRzPanelEditDlg.EdtBorderColorChange(Sender: TObject);
begin
  PnlPreview.BorderColor := EdtBorderColor.SelectedColor;
end;


procedure TRzPanelEditDlg.EdtBorderHighlightChange(Sender: TObject);
begin
  PnlPreview.BorderHighlight := EdtBorderHighlight.SelectedColor;
end;


procedure TRzPanelEditDlg.EdtBorderShadowChange(Sender: TObject);
begin
  PnlPreview.BorderShadow := EdtBorderShadow.SelectedColor;
end;


end.
