{===============================================================================
  RzTabStopEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzTabStopProperty
    Displays special dialog to manage tabstops for the TRzTabbedListBox.

  TRzTabbedListBoxEditor
    Adds context menu to edit the tab stops, add items, and set the Align
    property.


  Modification History
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Modified the TabStop Editor so that it is resizable.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Increased the range of tab stop positions in the Tab Stop Editor from 100
      to 200.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added TabStopsMode options to Tab Stop dialog box.
    * Added "Manual Tab Stops" and "Automatic Tab Stops" items to context menu
      for TRzTabbedListBox.
    * Added "Show Groups" item to context menu for TRzTabbedListBox.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
===============================================================================}

{$I RzComps.inc}

unit RzTabStopEditor;

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
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  Menus,
  RzDesignEditors,
  RzTrkBar,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzLstBox,
  RzRadChk,
  RzLabel,
  RzButton,
  ExtCtrls,
  RzPanel,
  RzRadGrp;

type
  TRzTabStopProperty = class( TPropertyEditor )
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;


  TRzTabbedListBoxEditor = class( TRzDefaultEditor )
  protected
    function ListBox: TRzTabbedListBox;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzTabStopEditDlg = class(TForm)
    btnOK: TRzButton;
    btnCancel: TRzButton;
    grpPreview: TRzGroupBox;
    grpTabStops: TRzGroupBox;
    lstTabs: TRzListBox;
    lblMin: TRzLabel;
    lblMax: TRzLabel;
    Label3: TRzLabel;
    lblTabNum: TRzLabel;
    lstPreview: TRzTabbedListBox;
    btnAdd: TRzButton;
    btnDelete: TRzButton;
    chkRightAligned: TRzCheckBox;
    trkTabPos: TRzTrackBar;
    grpTabStopsMode: TRzRadioGroup;
    RzPanel1: TRzPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lstTabsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure trkTabPosChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkRightAlignedClick(Sender: TObject);
    procedure grpTabStopsModeClick(Sender: TObject);
  private
    FUpdating: Boolean;
  end;


implementation

{$R *.dfm}

uses
  RzCommon;

{========================}
{== TRzTabStopProperty ==}
{========================}

function TRzTabStopProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog ];
end;


function TRzTabStopProperty.GetValue: string;
begin
  Result := Format( '(%s)', [ GetPropType^.Name ] );       // Display Type Name in Object Inspector
end;


procedure TRzTabStopProperty.Edit;
var
  Dlg: TRzTabStopEditDlg;
  OwnerName: string;
  I: Integer;

  procedure CopyList( Dest, Source: TRzTabbedListBox );
  begin
    { Set Preview List Box strings and Font to be same as component's }
    Dest.Items := Source.Items;
    Dest.Font := Source.Font;
    Dest.GroupFont := Source.GroupFont;
    Dest.GroupColor := Source.GroupColor;
    Dest.UseGradients := Source.UseGradients;
    Dest.TabStopsMode := Source.TabStopsMode;
  end;

begin
  Dlg := TRzTabStopEditDlg.Create( Application );
  try
    if TComponent( GetComponent( 0 ) ).Owner <> nil then
      OwnerName := TComponent( GetComponent(0) ).Owner.Name + '.'
    else
      OwnerName := '';
    Dlg.Caption := OwnerName + TComponent( GetComponent(0) ).Name + '.' + GetName + ' - ' + Dlg.Caption;

    CopyList( Dlg.lstPreview, TRzTabbedListBox( GetComponent( 0 ) ) );

    Dlg.lstPreview.TabStops := TRzTabStopList( GetOrdValue );

    // Add preset tabs to the lstTabs list box
    for I := 0 to Dlg.lstPreview.TabStops.Count - 1 do
      Dlg.lstTabs.Items.Add( IntToStr( Abs( Dlg.lstPreview.TabStops[ I ] ) ) );

    Dlg.grpTabStopsMode.ItemIndex := Ord( Dlg.lstPreview.TabStopsMode );

    if Dlg.ShowModal = mrOK then
    begin                                                  // If user presses OK, move TabList from Dlg to Property
      SetOrdValue( Longint( Dlg.lstPreview.TabStops ) );
      TRzTabbedListBox( GetComponent( 0 ) ).TabStopsMode := Dlg.lstPreview.TabStopsMode;
    end;
  finally
    Dlg.Free;
  end;
end; {= TRzTabStopProperty.Edit =}



{====================================}
{== TRzTabbedListBoxEditor Methods ==}
{====================================}

function TRzTabbedListBoxEditor.ListBox: TRzTabbedListBox;
begin
  Result := Component as TRzTabbedListBox;
end;


function TRzTabbedListBoxEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;


function TRzTabbedListBoxEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Items...';
    1: Result := '-';
    2: Result := 'Edit Tab Stops...';
    3: Result := 'Manual Tab Stops';
    4: Result := 'Automatic Tab Stops';
    5: Result := '-';
    6: Result := 'Align';
    7: Result := '-';
    8: Result := 'Show Groups';
  end;
end;


function TRzTabbedListBoxEditor.AlignMenuIndex: Integer;
begin
  Result := 6;
end;


procedure TRzTabbedListBoxEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := ListBox.TabStopsMode = tsmManual;
    4: Item.Checked := ListBox.TabStopsMode = tsmAutomatic;
    8: Item.Checked := ListBox.ShowGroups;
  end;
end;


procedure TRzTabbedListBoxEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0: EditPropertyByName( 'Items' );
    2: EditPropertyByName( 'TabStops' );

    3: ListBox.TabStopsMode := tsmManual;
    4: ListBox.TabStopsMode := tsmAutomatic;
    8: ListBox.ShowGroups := not ListBox.ShowGroups;
  end;
  if Index in [ 3, 4, 8 ] then
    DesignerModified;
end;



{===============================}
{== TRzTabStopEditDlg Methods ==}
{===============================}

procedure TRzTabStopEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  FUpdating := False;
end;


procedure TRzTabStopEditDlg.btnAddClick(Sender: TObject);
var
  NewTab: Word;
  Idx   : Integer;
begin
  NewTab := 8;
  if lstPreview.TabStops.Count > 0 then
  begin               { Add a new tab stop 8 positions after the last tab stop }
    NewTab := Abs( lstPreview.TabStops[ lstPreview.TabStops.Count - 1 ] ) + 8;
  end;
  Idx := lstTabs.Items.Add( IntToStr( NewTab ) );{ Add TabStop to Editing List }
  lstPreview.TabStops.Add( NewTab );            { Add Tab stop to Preview list }

  lstTabs.ItemIndex := Idx;                  { Select the newly added tab stop }
  lstTabsClick( nil );                                      { Update Track Bar }
  BtnDelete.Enabled := True;
  trkTabPos.Enabled := True;
end;


procedure TRzTabStopEditDlg.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := lstTabs.ItemIndex;
  lstPreview.TabStops.Delete( Index );              { Remove selected Tab Stop }
  lstTabs.Items.Delete( Index );

  if lstTabs.Items.Count > 0 then
  begin
    if Index = lstTabs.Items.Count then
      Dec( Index );
    lstTabs.ItemIndex := Index;
    lstTabsClick( nil );                                    { Update Track Bar }
  end
  else
  begin
    BtnDelete.Enabled := False;
    trkTabPos.Enabled := False;
  end;
end;


{===============================================================================
  TRzTabStopEditDlg.lstTabsClick

  This method updates the Min and Max values of the track bar to reflect
  the range a particular tab stop may move.  A tab stop may not move past
  a tab stop that exists before or after it in the list.  Setting the
  track bar range ensures that this cannot happen. The FUpdating flag is
  set at the beginning of this method to prevent a change in the
  TrackBar's Min or Max value from causing the trkTabPosChange event from
  altering the display.
===============================================================================}

procedure TRzTabStopEditDlg.lstTabsClick(Sender: TObject);
var
  I: Integer;
begin
  FUpdating := True;
  I := lstTabs.ItemIndex;
  trkTabPos.Min := 0;
  trkTabPos.Max := 200;

  if lstTabs.ItemIndex > 0 then                  { Get Previous Tab Stop Value }
    trkTabPos.Min := Abs( StrToInt( lstTabs.Items[ I - 1 ] ) )
  else
    trkTabPos.Min := 0;

  if lstTabs.ItemIndex < lstTabs.Items.Count - 1 then     { Get Next Tab Value }
    trkTabPos.Max := Abs( StrToInt( lstTabs.Items[ I + 1 ] ) )
  else
    trkTabPos.Max := 200;

  { Update TrackBar position to reflect currently selected tab stop }
  trkTabPos.Position := Abs( lstPreview.TabStops[ I ] );
  chkRightAligned.Checked := lstPreview.TabStops[ I ] < 0;
  LblTabNum.Caption := IntToStr( I + 1 );
  LblMin.Caption := IntToStr( trkTabPos.Min );
  LblMax.Caption := IntToStr( trkTabPos.Max );
  BtnDelete.Enabled := True;
  trkTabPos.Enabled := True;
  FUpdating := False;
end;


procedure TRzTabStopEditDlg.FormShow(Sender: TObject);
begin
  if ( lstTabs.Items.Count > 0 ) and ( lstPreview.TabStopsMode = tsmManual ) then
  begin
    lstTabs.ItemIndex := 0;
    lstTabsClick( nil );
    BtnDelete.Enabled := True;
    trkTabPos.Enabled := True;
  end;
end;


{===============================================================================
  TRzTabStopEditDlg.trkTabPosChange

  As the track bar is moved, the value of the selected tab stop is updated.
  The change is immediately reflected in the Tab and Preview List boxes.
===============================================================================}

procedure TRzTabStopEditDlg.trkTabPosChange(Sender: TObject);
var
  I: Integer;
begin
  if not FUpdating then
  begin
    I := lstTabs.ItemIndex;
    if chkRightAligned.Checked then
      lstPreview.TabStops[ I ] := -trkTabPos.Position
    else
      lstPreview.TabStops[ I ] := trkTabPos.Position;
    lstTabs.Items[ I ] := IntToStr( trkTabPos.Position );
    lstTabs.ItemIndex := I;
  end;
end;

procedure TRzTabStopEditDlg.chkRightAlignedClick(Sender: TObject);
var
  I: Integer;
begin
  if not FUpdating then
  begin
    I := lstTabs.ItemIndex;
    if chkRightAligned.Checked then
      lstPreview.TabStops[ I ] := -trkTabPos.Position
    else
      lstPreview.TabStops[ I ] := trkTabPos.Position;
  end;
end;


procedure TRzTabStopEditDlg.grpTabStopsModeClick(Sender: TObject);
var
  I: Integer;
begin
  lstPreview.TabStopsMode := TRzTabStopsMode( grpTabStopsMode.ItemIndex );
  grpTabStops.Enabled := lstPreview.TabStopsMode = tsmManual;

  // Reset TabStops list
  lstTabs.Clear;
  for I := 0 to lstPreview.TabStops.Count - 1 do
    lstTabs.Items.Add( IntToStr( Abs( lstPreview.TabStops[ I ] ) ) );
end;

end.



