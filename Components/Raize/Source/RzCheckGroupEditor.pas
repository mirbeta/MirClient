{===============================================================================
  RzCheckGroupEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzCheckGroupEditor
    Adds context menu to TRzCheckGroup to quickly add items


  Modification History
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new GroupStyle menu item to design context menu of TRzCheckGroup
      that handles the new GroupStyle values added to the control.
    * Added VisualStyle and GradientColorStyle menu items to designer context
      menu of TRzCheckGroup.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * In XP Colors context menu item event handler set ItemHotTrackColorType to
      htctActual.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzCheckGroupEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Controls,
  Graphics,
  Forms,
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Classes,
  Dialogs,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzDesignEditors,
  RzRadGrp,
  RzPanel,
  RzTrkBar,
  RzEdit,
  Mask,
  RzLabel,
  RzButton;


type
  TRzCheckGroupEditor = class( TRzDefaultEditor )
  protected
    function CheckGroup: TRzCheckGroup; virtual;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ItemsMenuHandler( Sender: TObject );
    procedure GroupStyleMenuHandler( Sender: TObject );
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzCheckGroupEditDlg = class(TForm)
    PnlButtons: TRzPanel;
    PnlOptions: TRzPanel;
    EdtCaption: TRzEdit;
    Label1: TRzLabel;
    EdtItems: TRzMemo;
    Label2: TRzLabel;
    BtnLoad: TRzButton;
    Label3: TRzLabel;
    BtnClear: TRzButton;
    DlgOpen: TOpenDialog;
    TrkColumns: TRzTrackBar;
    PnlPreview: TRzPanel;
    GrpPreview: TRzCheckGroup;
    RzPanel1: TRzPanel;
    BtnOk: TRzButton;
    BtnCancel: TRzButton;
    procedure EdtCaptionChange(Sender: TObject);
    procedure TrkColumnsChange(Sender: TObject);
    procedure EdtItemsChange(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure TrkColumnsDrawTick(TrackBar: TRzTrackBar; Canvas: TCanvas;
      Location: TPoint; Index: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FUpdating: Boolean;
  public
    procedure UpdateControls;
  end;


implementation

{$R *.dfm}

uses
  SysUtils,
  RzCommon;

{=================================}
{== TRzCheckGroupEditor Methods ==}
{=================================}

function TRzCheckGroupEditor.CheckGroup: TRzCheckGroup;
begin
  // Helper function to provide quick access to component being edited.
  // Also makes sure Component is a TRzCheckGroup
  Result := Component as TRzCheckGroup;
end;


function TRzCheckGroupEditor.GetVerbCount: Integer;
begin
  // Return the number of new menu items to display
  if CheckGroup.Items.Count > 0 then
    Result := 9
  else
    Result := 7;
end;


function TRzCheckGroupEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Items...';
    1: Result := 'HotTrack Items';
    2: Result := 'XP Colors';
    3: Result := '-';
    4: Result := 'Group Style';
    5: Result := 'Visual Style';
    6: Result := 'Gradient Color Style';
    7: Result := '-';
    8: Result := 'Check Item';

  end;
end;


procedure TRzCheckGroupEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I: Integer;

  procedure CreateItemMenu( Index: Integer; const Caption: string; Checked: Boolean );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Index;
    NewItem.Checked := Checked;
    NewItem.OnClick := ItemsMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    1: Item.Checked := CheckGroup.ItemHotTrack;

    4: // GroupStyle
    begin
      CreateGroupStyleMenuItem( Item, gsFlat, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsStandard, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsTopLine, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsBanner, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsUnderline, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsCustom, CheckGroup.GroupStyle,
                                GroupStyleMenuHandler );
    end;

    5: // VisualStyle
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, CheckGroup.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, CheckGroup.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, CheckGroup.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    6: // GradientColorStyle
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, CheckGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, CheckGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, CheckGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;

    8:
    begin
      for I := 0 to CheckGroup.Items.Count - 1 do
        CreateItemMenu( I, CheckGroup.Items[ I ], CheckGroup.ItemChecked[ I ] );
    end;
  end;

end; {= TRzCheckGroupEditor.PrepareMenuItem =}


procedure TRzCheckGroupEditor.ExecuteVerb( Index: Integer );
var
  D: TRzCheckGroupEditDlg;

  procedure CopyCheckGroup( Dest, Source: TRzCheckGroup );
  var
    I: Integer;
  begin
    Dest.Caption := Source.Caption;
    Dest.Columns := Source.Columns;
    Dest.Items := Source.Items;
    Dest.GroupStyle := Source.GroupStyle;
    Dest.ItemFrameColor := Source.ItemFrameColor;
    Dest.ItemHotTrack := Source.ItemHotTrack;
    Dest.ItemHighlightColor := Source.ItemHighlightColor;
    Dest.ItemHotTrackColor := Source.ItemHotTrackColor;
    Dest.ItemHotTrackColorType := Source.ItemHotTrackColorType;
    Dest.ItemFont := Source.ItemFont;
    for I := 0 to Dest.Items.Count - 1 do
      Dest.ItemChecked[ I ] := Source.ItemChecked[ I ];
  end;

begin
  case Index of
    0:
    begin
      D := TRzCheckGroupEditDlg.Create( Application );
      try
        // Copy component attributes to the GrpPreview component
        CopyCheckGroup( D.GrpPreview, CheckGroup );

        // Set the dialog's Caption to reflect component being edited
        D.Caption := Component.Owner.Name +'.'+ Component.Name + D.Caption;

        D.UpdateControls;              // Update all controls on dialog box

        if D.ShowModal = mrOK then
        begin
          CopyCheckGroup( CheckGroup, D.GrpPreview );
          // Tell the Form Designer to set the Modified flag for the form
          DesignerModified;
        end;
      finally
        D.Free;
      end;
    end;

    1:
    begin
      CheckGroup.ItemHotTrack := not CheckGroup.ItemHotTrack;
      DesignerModified;
    end;

    2: // XP Colors
    begin
      CheckGroup.ItemHotTrack := True;
      CheckGroup.ItemHotTrackColorType := htctActual;
      CheckGroup.ItemHighlightColor := xpRadChkMarkColor;
      CheckGroup.ItemHotTrackColor := xpHotTrackColor;
      CheckGroup.ItemFrameColor := xpRadChkFrameColor;
      DesignerModified;
    end;

  end;
end; {= TRzCheckGroupEditor.ExecuteVerb =}


procedure TRzCheckGroupEditor.ItemsMenuHandler( Sender: TObject );
var
  Idx: Integer;
begin
  Idx := TMenuItem( Sender ).Tag;
  CheckGroup.ItemChecked[ Idx ] := not CheckGroup.ItemChecked[ Idx ];
  DesignerModified;
end;


procedure TRzCheckGroupEditor.GroupStyleMenuHandler( Sender: TObject );
begin
  CheckGroup.GroupStyle := TRzGroupBoxStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzCheckGroupEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  CheckGroup.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzCheckGroupEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  CheckGroup.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


{==================================}
{== TRzCheckGroupEditDlg Methods ==}
{==================================}

procedure TRzCheckGroupEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ICON' );

  Width := MulDiv( Width, Screen.PixelsPerInch, 96 );
  Height := MulDiv( Height, Screen.PixelsPerInch, 96 );

  EdtItems.Anchors := [ akLeft, akTop, akBottom];
end;


procedure TRzCheckGroupEditDlg.UpdateControls;
begin
  FUpdating := True;
  try
    EdtCaption.Text := GrpPreview.Caption;
    TrkColumns.Position := GrpPreview.Columns;
    EdtItems.Lines := GrpPreview.Items;
  finally
    FUpdating := False;
  end;
end;

procedure TRzCheckGroupEditDlg.EdtCaptionChange(Sender: TObject);
begin
  GrpPreview.Caption := EdtCaption.Text;
end;


procedure TRzCheckGroupEditDlg.TrkColumnsChange(Sender: TObject);
begin
  GrpPreview.Columns := TrkColumns.Position;
end;


procedure TRzCheckGroupEditDlg.EdtItemsChange(Sender: TObject);
begin
  if not FUpdating then
    GrpPreview.Items := EdtItems.Lines;
end;


procedure TRzCheckGroupEditDlg.BtnLoadClick(Sender: TObject);
begin
  if DlgOpen.Execute then
    EdtItems.Lines.LoadFromFile( DlgOpen.FileName );
end;


procedure TRzCheckGroupEditDlg.BtnClearClick(Sender: TObject);
begin
  EdtItems.Lines.Clear;
  GrpPreview.Items.Clear;
end;


procedure TRzCheckGroupEditDlg.TrkColumnsDrawTick(TrackBar: TRzTrackBar;
  Canvas: TCanvas; Location: TPoint; Index: Integer);
var
  S: string;
  W: Integer;
begin
  Canvas.Brush.Color := TrackBar.Color;
  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Size := 7;
  Canvas.Font.Style := [];
  S := IntToStr( Index );
  W := Canvas.TextWidth( S );
  Canvas.TextOut( Location.X - (W div 2), 1, S );
end;


end.

