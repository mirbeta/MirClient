{===============================================================================
  RzDBRadioGroupEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzDBRadioGroupEditor 
    Adds context menu to TRzDBRadioGroup to quickly add items and values.


  Modification History
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new GroupStyle menu item to design context menu of TRzDBRadioGroup
      that handles the new GroupStyle values added to the control.
    * Added VisualStyle and GradientColorStyle menu items to designer context
      menu of TRzDBRadioGroup.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Modified the TRzDBRadioGroup editor form to utilize the grid approach
      of entering items and values that was created for the TRzComboBox and
      TRzDBComboBox controls.  As such, Items and corresponding Values can be
      entered simulatenously into a grid.  Alternatively, the user can click the
      Load button and specify a text file that contains the items. If the text
      file is structured in item=value format, both columns of the grid will be
      populated appropriately.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * In XP Colors context menu item event handler set ItemHotTrackColorType to
      htctActual.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzDBRadioGroupEditor;

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
  RzDBDesignEditors,
  RzRadGrp,
  RzPanel,
  RzTrkBar,
  Grids,
  RzDBRGrp,
  RzEdit,
  Mask,
  RzLabel,
  RzButton, 
  RzGrids;


type
  TRzDBRadioGroupEditor = class( TRzDBControlEditor )
  protected
    function RadioGroup: TRzDBRadioGroup;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure GroupStyleMenuHandler( Sender: TObject );
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzDBRadioGroupEditDlg = class(TForm)
    pnlButtons: TRzPanel;
    PnlOptions: TRzPanel;
    btnLoad: TRzButton;
    btnClear: TRzButton;
    dlgOpen: TOpenDialog;
    pnlPreview: TRzPanel;
    grpPreview: TRzDBRadioGroup;
    RzPanel1: TRzPanel;
    btnOk: TRzButton;
    btnCancel: TRzButton;
    grdItemsValues: TRzStringGrid;
    Label1: TRzLabel;
    edtCaption: TRzEdit;
    Label2: TRzLabel;
    trkColumns: TRzTrackBar;
    procedure edtCaptionChange(Sender: TObject);
    procedure trkColumnsChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure trkColumnsDrawTick(TrackBar: TRzTrackBar; Canvas: TCanvas;
      Location: TPoint; Index: Integer);
    procedure FormCreate(Sender: TObject);
    procedure grdItemsValuesResize(Sender: TObject);
    procedure grdItemsValuesClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure ResetGrid;
  public
    procedure UpdateControls;
    procedure UpdateGroup;
  end;


implementation

{$R *.dfm}

uses
  SysUtils,
  RzCommon;

{===================================}
{== TRzDBRadioGroupEditor Methods ==}
{===================================}

function TRzDBRadioGroupEditor.RadioGroup: TRzDBRadioGroup;
begin
  // Helper function to provide quick access to component being edited.
  // Also makes sure Component is a TRzDBRadioGroup
  Result := Component as TRzDBRadioGroup;
end;


function TRzDBRadioGroupEditor.GetVerbCount: Integer;
begin
  // Return the number of new menu items to display
  Result := 10;
end;


function TRzDBRadioGroupEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Edit Items && Values...';
    4: Result := 'HotTrack Items';
    5: Result := 'XP Colors';
    6: Result := '-';
    7: Result := 'Group Style';
    8: Result := 'Visual Style';
    9: Result := 'Gradient Color Style';
  end;
end;


procedure TRzDBRadioGroupEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    4: Item.Checked := RadioGroup.ItemHotTrack;

    7: // GroupStyle
    begin
      CreateGroupStyleMenuItem( Item, gsFlat, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsStandard, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsTopLine, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsBanner, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsUnderline, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsCustom, RadioGroup.GroupStyle,
                                GroupStyleMenuHandler );
    end;

    8: // VisualStyle
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, RadioGroup.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, RadioGroup.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, RadioGroup.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    9: // GradientColorStyle
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, RadioGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, RadioGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, RadioGroup.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;
  end;
end;


procedure TRzDBRadioGroupEditor.ExecuteVerb( Index: Integer );
var
  D: TRzDBRadioGroupEditDlg;

  procedure CopyRadioGroup( Dest, Source: TRzDBRadioGroup );
  begin
    Dest.Caption := Source.Caption;
    Dest.Columns := Source.Columns;
    Dest.Items := Source.Items;
    Dest.Values := Source.Values;
    Dest.GroupStyle := Source.GroupStyle;
    Dest.ItemFrameColor := Source.ItemFrameColor;
    Dest.ItemHotTrack := Source.ItemHotTrack;
    Dest.ItemHighlightColor := Source.ItemHighlightColor;
    Dest.ItemHotTrackColor := Source.ItemHotTrackColor;
    Dest.ItemHotTrackColorType := Source.ItemHotTrackColorType;
  end;

begin
  case Index of
    3:
    begin
      D := TRzDBRadioGroupEditDlg.Create( Application );
      try
        // Copy component attributes to the GrpPreview component
        CopyRadioGroup( D.GrpPreview, RadioGroup );

        // Set the dialog's Caption to reflect component being edited
        D.Caption := Component.Owner.Name +'.'+ Component.Name + D.Caption;

        D.UpdateControls;              // Update all controls on dialog box

        if D.ShowModal = mrOK then
        begin
          CopyRadioGroup( RadioGroup, D.GrpPreview );
          // Tell the Form Designer to set the Modified flag for the form
          DesignerModified;
        end;
      finally
        D.Free;
      end;
    end;

    4:
    begin
      RadioGroup.ItemHotTrack := not RadioGroup.ItemHotTrack;
      DesignerModified;
    end;

    5: // XP Colors
    begin
      RadioGroup.ItemHotTrack := True;
      RadioGroup.ItemHotTrackColorType := htctActual;
      RadioGroup.ItemHighlightColor := xpRadChkMarkColor;
      RadioGroup.ItemHotTrackColor := xpHotTrackColor;
      RadioGroup.ItemFrameColor := xpRadChkFrameColor;
      DesignerModified;
    end;
  end;
end; {= TRzDBRadioGroupEditor.ExecuteVerb =}


procedure TRzDBRadioGroupEditor.GroupStyleMenuHandler( Sender: TObject );
begin
  RadioGroup.GroupStyle := TRzGroupBoxStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzDBRadioGroupEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  RadioGroup.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzDBRadioGroupEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  RadioGroup.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{==================================}
{== TRzRadioGroupEditDlg Methods ==}
{==================================}

procedure TRzDBRadioGroupEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ICON' );

  Width := MulDiv( Width, Screen.PixelsPerInch, 96 );
  Height := MulDiv( Height, Screen.PixelsPerInch, 96 );
end;


procedure TRzDBRadioGroupEditDlg.UpdateControls;
var
  I: Integer;
begin
  FUpdating := True;
  try
    edtCaption.Text := grpPreview.Caption;
    trkColumns.Position := grpPreview.Columns;

    ResetGrid;
    grdItemsValues.RowCount := Max( grpPreview.Items.Count + 1,
                                    grdItemsValues.RowCount );

    for I := 0 to grpPreview.Items.Count - 1 do
      grdItemsValues.Cells[ 1, I + 1 ] := grpPreview.Items[ I ];

    for I := 0 to grpPreview.Values.Count - 1 do
      grdItemsValues.Cells[ 2, I + 1 ] := grpPreview.Values[ I ];

  finally
    FUpdating := False;
  end;
end;


procedure TRzDBRadioGroupEditDlg.UpdateGroup;
var
  I: Integer;
  ItemStr: string;
begin
  grpPreview.Items.Clear;
  grpPreview.Values.Clear;
  for I := 1 to grdItemsValues.RowCount - 1 do
  begin
    ItemStr := Trim( grdItemsValues.Cells[ 1, I ] );
    if ItemStr <> '' then
    begin
      grpPreview.AddItemValue( ItemStr, Trim( grdItemsValues.Cells[ 2, I ] ) );
    end
    else
    begin
      // At first blank item, then break out of loop;
      Break;
    end;
  end;
end;


procedure TRzDBRadioGroupEditDlg.btnLoadClick(Sender: TObject);
var
  FileContents: TStringList;
  S: string;
  I: Integer;
begin
  if dlgOpen.Execute then
  begin
    FileContents := TStringList.Create;
    try
      FileContents.LoadFromFile( dlgOpen.FileName );

      ResetGrid;
      grdItemsValues.RowCount := Max( FileContents.Count + 1,
                                      grdItemsValues.RowCount );

      for I := 0 to FileContents.Count - 1 do
      begin
        S := FileContents.Names[ I ];
        if S <> '' then
        begin
          grdItemsValues.Cells[ 1, I + 1 ] := S;
          grdItemsValues.Cells[ 2, I + 1 ] := FileContents.ValueFromIndex[ I ];
        end
        else
          grdItemsValues.Cells[ 1, I + 1 ] := FileContents[ I ];
      end;
    finally
      FileContents.Free;
    end;

    UpdateGroup;
  end;
end; {= TRzDBRadioGroupEditDlg.btnLoadClick =}


procedure TRzDBRadioGroupEditDlg.ResetGrid;
begin
  grdItemsValues.Cols[ 1 ].Clear;
  grdItemsValues.Cols[ 2 ].Clear;
  grdItemsValues.Cells[ 1, 0 ] := 'Item';
  grdItemsValues.Cells[ 2, 0 ] := 'Value';
  grdItemsValues.RowCount := 1024;
end;


procedure TRzDBRadioGroupEditDlg.edtCaptionChange(Sender: TObject);
begin
  GrpPreview.Caption := EdtCaption.Text;
end;


procedure TRzDBRadioGroupEditDlg.trkColumnsChange(Sender: TObject);
begin
  GrpPreview.Columns := TrkColumns.Position;
end;


procedure TRzDBRadioGroupEditDlg.btnClearClick(Sender: TObject);
begin
  ResetGrid;
  GrpPreview.Items.Clear;
  GrpPreview.Values.Clear;
end;


procedure TRzDBRadioGroupEditDlg.trkColumnsDrawTick(TrackBar: TRzTrackBar;
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


procedure TRzDBRadioGroupEditDlg.grdItemsValuesResize(Sender: TObject);
var
  W: Integer;
begin
  W := ( grdItemsValues.ClientWidth - 10 - 1 ) div 3;
  grdItemsValues.ColWidths[ 0 ] := 10;
  grdItemsValues.ColWidths[ 1 ] := W * 2;
  grdItemsValues.ColWidths[ 2 ] := W;
end;


procedure TRzDBRadioGroupEditDlg.grdItemsValuesClick(Sender: TObject);
begin
  if not FUpdating then
    UpdateGroup;
end;


procedure TRzDBRadioGroupEditDlg.btnOkClick(Sender: TObject);
begin
  // Call UpdateGroup to make sure that the last item (i.e. cell) entered
  // by user gets transfered into the grpPreview control.
  UpdateGroup;
end;

end.



