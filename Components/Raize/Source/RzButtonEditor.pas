{===============================================================================
  RzButtonEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzButtonEditor
    Adds context menu and editing dialog.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
===============================================================================}

{$I RzComps.inc}

unit RzButtonEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  Mask,
  RzEdit,
  Menus,
  Buttons,
  RzCommon,
  RzSpnEdt,
  RzDesignEditors,
  RzRadChk,
  RzLabel,
  RzRadGrp,
  RzButton,
  RzPanel;

type
  TRzButtonEditor = class( TRzDefaultEditor )
  protected
    function Button: TRzButton;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzButtonEditDlg = class(TForm)
    GrpPreview: TRzGroupBox;
    BtnPreview: TRzButton;
    BtnOK: TRzButton;
    BtnCancel: TRzButton;
    GrpModalResult: TRzRadioGroup;
    GrpSize: TRzGroupBox;
    OptStandardSize: TRzRadioButton;
    OptLargeSize: TRzRadioButton;
    OptCustomSize: TRzRadioButton;
    LblWidth: TRzLabel;
    LblHeight: TRzLabel;
    GrpSpecial: TRzGroupBox;
    BtnOKTemplate: TRzButton;
    BtnCancelTemplate: TRzButton;
    BtnHelpTemplate: TRzButton;
    GrpKeyboard: TRzGroupBox;
    ChkDefault: TRzCheckBox;
    ChkCancel: TRzCheckBox;
    GrpCaption: TRzGroupBox;
    EdtCaption: TRzEdit;
    Label1: TRzLabel;
    Label4: TRzLabel;
    Label5: TRzLabel;
    BtnYesTemplate: TRzButton;
    BtnNoTemplate: TRzButton;
    ChkEnabled: TRzCheckBox;
    SpnWidth: TRzSpinEdit;
    SpnHeight: TRzSpinEdit;
    SpnModalResult: TRzSpinEdit;
    procedure EdtCaptionChange(Sender: TObject);
    procedure ChkDefaultClick(Sender: TObject);
    procedure ChkCancelClick(Sender: TObject);
    procedure ButtonSizeClick(Sender: TObject);
    procedure ChkEnabledClick(Sender: TObject);
    procedure GrpModalResultClick(Sender: TObject);
    procedure BtnOKTemplateClick(Sender: TObject);
    procedure BtnCancelTemplateClick(Sender: TObject);
    procedure BtnHelpTemplateClick(Sender: TObject);
    procedure BtnYesTemplateClick(Sender: TObject);
    procedure BtnNoTemplateClick(Sender: TObject);
    procedure SpnWidthChange(Sender: TObject);
    procedure SpnHeightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure EnableWidthHeight( Enable: Boolean );
    procedure UpdateWidthHeight;
  public
    procedure UpdateControls;
  end;


implementation

{$R *.dfm}

uses
  Consts;                        

{=============================}
{== TRzButtonEditor Methods ==}
{=============================}

function TRzButtonEditor.Button: TRzButton;
begin
  Result := Component as TRzButton;
end;


function TRzButtonEditor.GetVerbCount: Integer;
begin
  Result := 8;
end;


function TRzButtonEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Button...';
    1: Result := '-';
    2: Result := 'HotTrack Style';
    3: Result := 'XP Colors';
    4: Result := '-';
    5: Result := SOKButton + ' Button Settings';
    6: Result := SCancelButton + ' Button Settings';
    7: Result := SHelpButton + ' Button Settings';
  end;
end;


procedure TRzButtonEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    2: Item.Checked := Button.HotTrack;
  end;
end;


{=========================================================================
  TRzButtonEditor.ExecuteVerb

  This method will be invoked by Delphi whenever the user selects one of
  the newly created item on the components context menu while in the
  Form Designer. If the first item is selected (i.e. Edit Button...), then
  the TRzButtonEditDlg form is displayed. If one of the other menu items
  is selected, the button's properties are changed appropriately.
=========================================================================}

procedure TRzButtonEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzButtonEditDlg;
  OwnerName: string;

  procedure CopyButton( Dest, Source: TRzButton );
  begin
    Dest.Caption := Source.Caption;
    Dest.ModalResult := Source.ModalResult;
    Dest.Default := Source.Default;
    Dest.Cancel := Source.Cancel;
    Dest.Width := Source.Width;
    Dest.Height := Source.Height;
    Dest.Enabled := Source.Enabled;
    Dest.HotTrack := Source.HotTrack;
    Dest.HotTrackColor := Source.HotTrackColor;
    Dest.HotTrackColorType := Source.HotTrackColorType;
    Dest.HighlightColor := Source.HighlightColor;
  end;

  procedure SetBtnProps( B: TRzButton; ACaption: TCaption; IsDefault, IsCancel: Boolean; AModalResult: TModalResult;
                         BtnWidth, BtnHeight: Integer; IsEnabled: Boolean );
  begin
    B.Caption := ACaption;
    B.Default := IsDefault;
    B.Cancel := IsCancel;
    B.ModalResult := AModalResult;
    B.Width := BtnWidth;
    B.Height := BtnHeight;
    B.Enabled := IsEnabled;
  end; {= SetBtnProps =}


begin
  case Index of
    0:                                        { User selected "Edit Button..." }
    begin
      Dlg := TRzButtonEditDlg.Create( Application );

      try
        { Copy component attributes to the BtnPreview component }
        CopyButton( Dlg.BtnPreview, Button );

        { Set the dialog's Caption to reflect component being edited }
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;

        Dlg.UpdateControls;                { Update all controls on dialog box }

        if Dlg.ShowModal = mrOK then                  { Display the dialog box }
        begin
          CopyButton( Button, Dlg.BtnPreview );
          DesignerModified;
        end;
      finally
        Dlg.Free;
      end;
    end;

    2:
    begin
      Button.HotTrack := not Button.HotTrack;
      DesignerModified;
    end;

    3: // XP Colors
    begin
      Button.HotTrack := True;
      Button.HotTrackColor := xpHotTrackColor;
      Button.HotTrackColorType := htctActual;
      Button.HighlightColor := clHighlight;
      Button.Color := xpButtonFaceColor;
      Button.FrameColor := xpButtonFrameColor;
      DesignerModified;
    end;

    5:                                                    { User selected "OK" }
    begin
      if Component is TRzBitBtn then
        TRzBitBtn( Component ).Kind := bkOk
      else
        SetBtnProps( Button, SOKButton, True, False, mrOK, 75, 25, True );
      DesignerModified;
    end;

    6:                                                { User selected "Cancel" }
    begin
      if Component is TRzBitBtn then
        TRzBitBtn( Component ).Kind := bkCancel
      else
        SetBtnProps( Button, SCancelButton, False, True, mrCancel, 75, 25, True );
      DesignerModified;
    end;

    7:                                                  { User selected "Help" }
    begin
      if Component is TRzBitBtn then
        TRzBitBtn( Component ).Kind := bkHelp
      else
        SetBtnProps( Button, SHelpButton, False, False, mrNone, 75, 25, True );
      DesignerModified;
    end;

  end; { case }
end; {= TRzButtonEditor.ExecuteVerb =}



{==============================}
{== TRzButtonEditDlg Methods ==}
{==============================}

procedure TRzButtonEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzButtonEditDlg.UpdateControls;
var
  W, H: Integer;
begin
  EdtCaption.Text := BtnPreview.Caption;
  if BtnPreview.ModalResult < 11 then
    GrpModalResult.ItemIndex := BtnPreview.ModalResult;
  SpnModalResult.Value := BtnPreview.ModalResult;
  ChkDefault.Checked := BtnPreview.Default;
  ChkCancel.Checked := BtnPreview.Cancel;
  ChkEnabled.Checked := BtnPreview.Enabled;


  W := BtnPreview.Width;
  H := BtnPreview.Height;

  SpnWidth.Value := W;
  SpnHeight.Value := H;

  if ( BtnPreview.Width = 75 ) and ( BtnPreview.Height = 25 ) then
    OptStandardSize.Checked := True
  else if ( BtnPreview.Width = 100 ) and ( BtnPreview.Height = 35 ) then
    OptLargeSize.Checked := True
  else
  begin
    OptCustomSize.Checked := True;
  end;
end; {= TRzButtonEditDlg.UpdateControls =}


procedure TRzButtonEditDlg.EdtCaptionChange(Sender: TObject);
begin
  BtnPreview.Caption := EdtCaption.Text;
end;

procedure TRzButtonEditDlg.ChkDefaultClick(Sender: TObject);
begin
  BtnPreview.Default := ChkDefault.Checked;
end;

procedure TRzButtonEditDlg.ChkCancelClick(Sender: TObject);
begin
  BtnPreview.Cancel := ChkCancel.Checked;
end;

procedure TRzButtonEditDlg.EnableWidthHeight( Enable: Boolean );
begin
  LblWidth.Enabled := Enable;
  SpnWidth.Enabled := Enable;
  LblHeight.Enabled := Enable;
  SpnHeight.Enabled := Enable;
end;


procedure TRzButtonEditDlg.UpdateWidthHeight;
begin
  if not ( csLoading in ComponentState ) then
  begin
    BtnPreview.Width := SpnWidth.IntValue;
    BtnPreview.Height := SpnHeight.IntValue;
  end;
end;


procedure TRzButtonEditDlg.ButtonSizeClick(Sender: TObject);
begin
  EnableWidthHeight( Sender = OptCustomSize );
  if Sender = OptStandardSize then
  begin
    SpnWidth.Value := 75;
    SpnHeight.Value := 25;
  end
  else if Sender = OptLargeSize then
  begin
    SpnWidth.Value := 100;
    SpnHeight.Value := 35;
  end
  else
  begin
    SpnWidth.Value := BtnPreview.Width;
    SpnHeight.Value := BtnPreview.Height;
  end;
  UpdateWidthHeight;
end;

procedure TRzButtonEditDlg.SpnWidthChange(Sender: TObject);
begin
  UpdateWidthHeight;
end;

procedure TRzButtonEditDlg.SpnHeightChange(Sender: TObject);
begin
  UpdateWidthHeight;
end;

procedure TRzButtonEditDlg.ChkEnabledClick(Sender: TObject);
begin
  BtnPreview.Enabled := ChkEnabled.Checked;
end;

procedure TRzButtonEditDlg.GrpModalResultClick(Sender: TObject);
begin
  SpnModalResult.Enabled := GrpModalResult.ItemIndex = 11;

  if GrpModalResult.ItemIndex < 11 then
  begin
    BtnPreview.ModalResult := TModalResult( GrpModalResult.ItemIndex );
    SpnModalResult.Value := GrpModalResult.ItemIndex;
  end
  else
    BtnPreview.ModalResult := SpnModalResult.IntValue;
end;

{ Special Buttons }

procedure TRzButtonEditDlg.BtnOKTemplateClick(Sender: TObject);
begin
  with BtnPreview do
  begin
    Caption := SOKButton;
    Default := True;
    Cancel := False;
    ModalResult := mrOK;
    Width := 75;
    Height := 25;
    Enabled := True;
  end;
  UpdateControls;
end;

procedure TRzButtonEditDlg.BtnCancelTemplateClick(Sender: TObject);
begin
  with BtnPreview do
  begin
    Caption := SCancelButton;
    Default := False;
    Cancel := True;
    ModalResult := mrCancel;
    Width := 75;
    Height := 25;
    Enabled := True;
  end;
  UpdateControls;

end;

procedure TRzButtonEditDlg.BtnHelpTemplateClick(Sender: TObject);
begin
  with BtnPreview do
  begin
    Caption := SHelpButton;
    Default := False;
    Cancel := False;
    ModalResult := mrNone;
    Width := 75;
    Height := 25;
    Enabled := True;
  end;
  UpdateControls;
end;

procedure TRzButtonEditDlg.BtnYesTemplateClick(Sender: TObject);
begin
  with BtnPreview do
  begin
    Caption := SYesButton;
    Default := False;
    Cancel := False;
    ModalResult := mrYes;
    Width := 75;
    Height := 25;
    Enabled := True;
  end;
  UpdateControls;
end;

procedure TRzButtonEditDlg.BtnNoTemplateClick(Sender: TObject);
begin
  with BtnPreview do
  begin
    Caption := SNoButton;
    Default := False;
    Cancel := False;
    ModalResult := mrNo;
    Width := 75;
    Height := 25;
    Enabled := True;
  end;
  UpdateControls;
end;


end.
 
