{***************************************************************************}
{ TDBAdvSmoothListBox component                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit DBAdvSmoothListBox;

interface

uses
  Windows, Classes, StdCtrls, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, Math,
  AdvSmoothListBox;

type
  TDBAdvSmoothListBox = class;

  TListBoxDataBinding = class(TPersistent)
  private
    FOwner: TDBAdvSmoothListBox;
    FOnChange: TNotifyEvent;
    FCaptionField: String;
    FGraphicRightField: string;
    FInfoField: string;
    FCheckedField: string;
    FHintField: string;
    FNotesField: string;
    FProgressValueField: string;
    FGraphicLeftField: string;
    FNotesTemplate: string;
    FCaptionTemplate: String;
    FKeyField: string;
    procedure SetCaptionField(const Value: String);
    procedure SetCheckedField(const Value: string);
    procedure SetGraphicLeftField(const Value: string);
    procedure SetGraphicRightField(const Value: string);
    procedure SetHintField(const Value: string);
    procedure SetInfoField(const Value: string);
    procedure SetNotesField(const Value: string);
    procedure SetProgressValueField(const Value: string);
    function GetDBListBox: TDBAdvSmoothListBox;
    procedure SetCaptionTemplate(const Value: String);
    procedure SetNotesTemplate(const Value: string);
    procedure SetKeyField(const Value: string);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TDBAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DBAdvSmoothListBox: TDBAdvSmoothListBox read GetDBListBox;
  published
    property CaptionField: String read FCaptionField write SetCaptionField;
    property CaptionTemplate: String read FCaptionTemplate write SetCaptionTemplate;
    property CheckedField: string read FCheckedField write SetCheckedField;  // must be boolean
    property NotesField: string read FNotesField write SetNotesField;
    property NotesTemplate: string read FNotesTemplate write SetNotesTemplate;
    property GraphicLeftField: string read FGraphicLeftField write SetGraphicLeftField;  // must be graphic
    property GraphicRightField: string read FGraphicRightField write SetGraphicRightField;  // must be graphic
    property HintField: string read FHintField write SetHintField;
    property InfoField: string read FInfoField write SetInfoField;
    property KeyField: string read FKeyField write SetKeyField;
    property ProgressValueField: string read FProgressValueField write SetProgressValueField;  // must be integer/float
  end;

  TAdvListBoxDataLink = class(TDataLink)
  private
    FListBox: TDBAdvSmoothListBox;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FLockEffects: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AListBox: TDBAdvSmoothListBox);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property ListBox: TDBAdvSmoothListBox read FListBox;
  end;

  TGetRecordCountEvent = procedure(Sender: TObject; var Count: integer) of object;
  TDataBindEvent = procedure (Sender: TObject; Item: TAdvSmoothListBoxItem; DBFields: TFields) of object;
  THTMLTemplateDataEvent = procedure(Sender: TObject; ItemIndex: Integer; FieldName: string; var Data: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSmoothListBox = class(TAdvSmoothListBox)
  private
    FDataLink: TAdvListBoxDataLink;
    FDataBinding: TListBoxDataBinding;
    FPageMode: Boolean;
    FOldTopIndex: Integer;
    FInternalChange: Boolean;
    FInternalSelectItem: Boolean;
    FOnDataBind: TDataBindEvent;
    FOnGetNotesTemplateData: THTMLTemplateDataEvent;
    FOnGetCaptionTemplateData: THTMLTemplateDataEvent;
    FOnGetRecordCount: TGetRecordCountEvent;
    procedure ActiveChange(Value: Boolean);
    procedure EditingChanged;
    procedure DataScroll(Distance: Integer);
    procedure RecordChanged(Field: TField);
    procedure DataChange;
    procedure UpdateData;
    procedure UpdateActiveItem;
    procedure UpdateTopIndex;
    procedure OnDataBindingChanged(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetDataBinding(const Value: TListBoxDataBinding);
    procedure SetPageMode(const Value: Boolean);
    function GetSelectedIndex: Integer;
    function HTMLDBReplace(s: string; Dataset: Tdataset; ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitPreview; override;
    procedure DoItemText(Sender: TObject; itemindex: integer; var itemcaption: String; var iteminfo: String; var itemnotes: String); override;
    procedure DoItemGraphics(Sender: TObject; itemindex: integer); override;
    procedure DoScroll(Sender: TObject; CurrentPosition, EndPosition: Double); override;
    procedure DoSmoothScroll(CurrentPosition, EndPosition: Double); override;
    procedure DoLookup(DispItem: TAdvSmoothListBoxDisplayListItem); override;
    procedure DoBoolPropertyChange(Item: TAdvSmoothListBoxItem; PropID: Integer; var Value: Boolean); override;
    procedure SetSelectionMode(const Value: TAdvSmoothListBoxSelectionMode); override;
    procedure DoInternalScroll; override;
    function LookupBarVisible: Boolean; override;
    function DoItemCheckToggle(Item: TAdvSmoothListBoxItem; GraphicLeft: Boolean; var Checked: Boolean): Boolean; override;
    procedure DoItemClick(Sender: TObject; itemindex: integer); override;
    procedure DoSelectItem(NewItemIndex: Integer); override;
    procedure SetMultiSelect(const Value: Boolean); override;
    function CheckDataSet: Boolean;
    function GetRecordCount: Integer;
    procedure UpdateItemCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure LoadFromDataSet;
    procedure Reload;
  published
    property DataBinding: TListBoxDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property PageMode: Boolean read FPageMode write SetPageMode default true;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True; // Can be shifted to parent class
    property OnDataBind: TDataBindEvent read FOnDataBind write FOnDataBind;
    property OnGetCaptionTemplateData: THTMLTemplateDataEvent read FOnGetCaptionTemplateData write FOnGetCaptionTemplateData;
    property OnGetNotesTemplateData: THTMLTemplateDataEvent read FOnGetNotesTemplateData write FOnGetNotesTemplateData;
    property OnGetRecordCount: TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
  end;


implementation

uses
  VDBConsts, Consts;

type
   TProAdvSmoothListBoxItem = class(TAdvSmoothListBoxItem);

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//------------------------------------------------------------------------------

{ TAdvListBoxDataLink }

constructor TAdvListBoxDataLink.Create(AListBox: TDBAdvSmoothListBox);
begin
  inherited Create;
  FListBox := AListBox;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

destructor TAdvListBoxDataLink.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.ActiveChanged;
begin
{$IFDEF DELPHI6_LVL}
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        raise exception.Create('UniDirectional DataSet');
{$ENDIF}
  if not FLockEffects then
    FListBox.ActiveChange(Active);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.Modified;
begin
  FModified := True;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.DataSetChanged;
begin
  if not FLockEffects then
    FListBox.DataChange;
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.DataSetScrolled(Distance: Integer);
begin
  if not FLockEffects then
    FListBox.DataScroll(Distance);
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.EditingChanged;
begin
  if not FLockEffects then
    FListBox.EditingChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.RecordChanged(Field: TField);
begin
  if not FLockEffects then
    FListBox.RecordChanged(Field);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified and not FLockEffects then
      FListBox.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

//------------------------------------------------------------------------------

{ TDBAdvSmoothListBox }

constructor TDBAdvSmoothListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TAdvListBoxDataLink.Create(Self);
  FDataLink.FListBox := Self;

  FDataBinding := TListBoxDataBinding.Create(Self);
  FDataBinding.OnChange := OnDataBindingChanged;
  ReadOnly := True;
  SetDisplayItemValues := True;
  FPageMode := True;
  FOldTopIndex := -1;
  SelectionMode := sPersistSelection;  // must be persist
end;

//------------------------------------------------------------------------------

destructor TDBAdvSmoothListBox.Destroy;
begin
  FDataBinding.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FOldTopIndex := GetTopIndex;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.MouseWheelHandler(var Message: TMessage);
var
  cp, cpt, i: Integer;
begin
  cp := GetPosition;
  inherited;
  cpt := GetPositionTo;
  DoScroll(Self, cp, cpt);

  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if (cp = 0) and (cpt = 0) then
      begin
        i := GetTopIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end
      else if (cp = cpt) then
      begin
        i := GetBottomIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end;     
    end;
  end;
  //OutputDebugString(PChar('---OP: ' + Inttostr(cp) + '  CPt: ' + Inttostr(cpt)));
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoScroll(Sender: TObject; CurrentPosition,
  EndPosition: Double);
var
  i: Integer;
  DirDown: Boolean;
begin
  inherited;

  if CheckDataSet and PageMode and not FInternalSelectItem then
  begin
    DirDown := CurrentPosition < EndPosition;

    if (CurrentPosition <> EndPosition) then
    begin
      if DirDown then
      begin
        i := GetBottomIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end
      else
      begin
        i := GetTopIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoLookup(DispItem: TAdvSmoothListBoxDisplayListItem);
var
  i: Integer;
begin
  inherited;

  if CheckDataSet and PageMode then
  begin
    i := GetBottomIndex - 1;  // 1 was extra added in GetBottomIndex
    if (i >= 0) and (i < Items.Count) then
      Items.SelectedItem := Items[i];
    i := GetTopIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
    Items.SelectedItem := DispItem.DisplayItem;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoInternalScroll;
var
  i: Integer;
begin
  inherited;

  if CheckDataSet and PageMode then
  begin
    i := GetBottomIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
    i := GetTopIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoSmoothScroll(CurrentPosition, EndPosition: Double);
var
  i: Integer;
  DirDown: Boolean;
begin
  inherited;

  if CheckDataSet and PageMode and not FInternalSelectItem then
  begin
    DirDown := CurrentPosition < EndPosition;
    //OutputDebugString(PChar('Dir: ' + BoolToStr(DirDown) ));

    if (CurrentPosition <> EndPosition) then
    begin
      if DirDown then
      begin
        i := GetBottomIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end
      else
      begin
        i := GetTopIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end;
    end;
    FOldTopIndex := GetTopIndex;
    //OutputDebugString(PChar('Top Index === ' + Inttostr(GetTopIndex) + ' Moved =' + inttostr(i)));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.UpdateTopIndex;
var
  i, SI: Integer;
begin
  if not CheckDataSet or not PageMode then
    Exit;

  i := GetTopIndex;
  SI := GetSelectedIndex;
  if (SI >= 0) and (SI - i <> FDataLink.ActiveRecord) then
  begin
    SetTopIndex(i + ((SI - i) - FDataLink.ActiveRecord));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoSelectItem(NewItemIndex: Integer);
var
  SI: Integer;
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

  if not CheckDataSet or not PageMode then
    Exit;

  if FInternalChange then
    Exit;

  SI := NewItemIndex + 1;

  if (SI > 0) then
  begin
    FInternalChange := True;
    FDataLink.DataSet.MoveBy(SI - FDataLink.DataSet.RecNo);
    FInternalChange := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DataScroll(Distance: Integer);
var
  RN: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) then
  begin
    FInternalChange := True;
    try
      Items.SelectedItem := Items[RN];
      UpdateTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.UpdateActiveItem;
var
  RN, SI: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  SI := GetSelectedIndex;
  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) and (SI <> RN) then
  begin
    FInternalChange := True;
    try
      Items.SelectedItem := Items[RN];
      UpdateTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoItemGraphics(Sender: TObject;
  itemindex: integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoBoolPropertyChange(Item: TAdvSmoothListBoxItem;
  PropID: Integer; var Value: Boolean);
begin
  inherited;
  case PropID of
    1, 2: Value := True;  // Item.Visible and Item.Enabled
    11:  //TAdvSmoothListBoxLookUpBar.Visible
    begin
      //if PageMode then
        //Value := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.HTMLDBReplace(s: string; Dataset: Tdataset;
  ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
begin
  beforetag := '';
  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        dbfld := DataSet.FieldByName(fld).DisplayText;
        if Assigned(DataEvent) then
          DataEvent(self, ItemIndex, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoItemText(Sender: TObject; itemindex: integer;
  var itemcaption, iteminfo, itemnotes: String);
var
  aField: TField;
  blobf: TBlobField;
  OldAR, {TopIdx, }SI, NAR: Integer;
  s: TStream;
  P: TNotifyEvent;
begin
  if PageMode and CheckDataSet then
  begin
    //TopIdx := GetTopIndex;
    OldAR := FDataLink.ActiveRecord;
    //FDataLink.ActiveRecord := Max(0, itemIndex - TopIdx);

    if not Assigned(Items.SelectedItem) then
      Exit;
    SI := Items.SelectedItem.Index;

    NAR := FDataLink.ActiveRecord;
    if (itemindex > SI) then
      NAR := FDataLink.ActiveRecord + ItemIndex - SI
    else if (itemindex < SI) then
      NAR := FDataLink.ActiveRecord + ItemIndex - SI;

    if (NAR < 0) or (NAR > FDataLink.BufferCount) then
      Exit;

    FDataLink.ActiveRecord := NAR;
    //FDataLink.ActiveRecord := Abs(ItemIndex - SI - FDataLink.ActiveRecord);
    try
      if (DataBinding.CaptionTemplate <> '') then
      begin
        itemCaption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, itemindex, FOnGetCaptionTemplateData);
      end
      else if (DataBinding.CaptionField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
        if Assigned(aField) then
          itemCaption := aField.Text;
      end;

      if (DataBinding.InfoField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.InfoField);
        if Assigned(aField) then
          itemInfo := aField.Text;
      end;

      if (DataBinding.NotesTemplate <> '') then
        itemNotes := HTMLDBReplace(DataBinding.NotesTemplate, FDataLink.DataSet, itemindex, FOnGetNotesTemplateData)
      else if (DataBinding.NotesField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.NotesField);
        if Assigned(aField) then
          itemNotes := aField.Text;
      end;

      if (DataBinding.CheckedField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
        if Assigned(aField) and (aField.DataType = ftBoolean) then
          TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalChecked(aField.AsBoolean);
      end;

      if (DataBinding.HintField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
        if Assigned(aField) then
          TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalHint(aField.Text);
      end;

      if (DataBinding.KeyField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.KeyField);
        if Assigned(aField) then
          TProAdvSmoothListBoxItem(Items[itemindex]).Key := aField.Text;
      end;


      if (DataBinding.ProgressValueField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.ProgressValueField);
        if Assigned(aField) then
        begin
          if (aField.DataType = ftInteger) then
            TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalProgressValue(aField.AsInteger)
          else if (aField.DataType = ftFloat) then
            TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalProgressValue(aField.AsFloat);
        end;
      end;

      if (DataBinding.GraphicLeftField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicLeftField);
        if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
        begin
          blobf := aField as TBlobField;
          s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
          P := Items[itemindex].GraphicLeft.OnChange;
          try
            s.Position := 0;
            Items[itemindex].GraphicLeft.OnChange := nil;
            Items[itemindex].GraphicLeft.LoadFromStream(s);
            //Items[itemindex].GraphicLeftType := gtImage;   // introducing CalcRect issue
          finally
            Items[itemindex].GraphicLeft.OnChange := p;
            s.Free;
          end;
        end;
      end;

      if (DataBinding.GraphicRightField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicRightField);
        if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
        begin
          blobf := aField as TBlobField;
          s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
          P := Items[itemindex].GraphicRight.OnChange;
          try
            s.Position := 0;
            Items[itemindex].GraphicRight.OnChange := nil;
            Items[itemindex].GraphicRight.LoadFromStream(s);
            //Items[itemindex].GraphicRightType := gtImage;   // introducing CalcRect issue
          finally
            Items[itemindex].GraphicRight.OnChange := P;
            s.Free;
          end;
        end;
      end;

    finally
      FDataLink.ActiveRecord := OldAR;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DataChange;
begin
  if not Assigned(FDataLink.DataSet) or not PageMode then
    Exit;

  UpdateItemCount;
  UpdateActiveItem;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.ActiveChange(Value: Boolean);
var
  rc, vic: Integer;
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if FDataLink.DataSet.Active then
      begin
        Items.Clear;
        if (not FDataLink.DataSet.IsSequenced and not Assigned(FOnGetRecordCount)) or not PageMode then
        begin
          PageMode := False;      // PageMode = true is not suitable with NonSequenced DataSets
          LoadFromDataSet;
        end
        else
        begin
          rc := GetRecordCount;
          ItemCount := rc;
          vic := GetVisibleItemCount;
          FDataLink.BufferCount := vic + 1;

          UpdateActiveItem;
        end;

        LookupBar.Visible := not LookupBar.Visible;
        LookupBar.Visible := not LookupBar.Visible;
      end
      else  // Active = False
      begin
        Items.Clear;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.DoItemClick(Sender: TObject; itemindex: integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetSelectionMode(
  const Value: TAdvSmoothListBoxSelectionMode);
begin
  if (Value <> sAutoDeselect) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.UpdateData;
begin
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.DoItemCheckToggle(Item: TAdvSmoothListBoxItem;
  GraphicLeft: Boolean; var Checked: Boolean): Boolean;
var
  aField: TField;
begin
  inherited DoItemCheckToggle(Item, GraphicLeft, Checked);
  Result := False;
  if not (csDesigning in ComponentState) and CheckDataSet and PageMode and Assigned(Item)
    and not Readonly and(DataBinding.CheckedField <> '') then
  begin
    aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
    if Assigned(aField) and (aField.DataType = ftBoolean) then
    begin
      if (GetSelectedIndex <> Item.Index) then
        SelectedItemIndex := Item.Index;
      if FDataLink.Edit then
      begin
        aField.AsBoolean := Checked;
        FDataLink.Modified;
        Result := True;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.RecordChanged(Field: TField);
begin
  if Assigned(Field) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetPageMode(const Value: Boolean);
begin
  if (FPageMode <> Value) then
  begin
    FPageMode := Value;
    if CheckDataSet then
      ActiveChange(FDataLink.DataSet.Active);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.LoadFromDataSet;
var
  aField: TField;
  blobf: TBlobField;
  s: TStream;
  P: TNotifyEvent;
  aItem: TAdvSmoothListBoxItem;
begin
  if CheckDataSet {and not PageMode} then
  begin
    FDataLink.DataSet.DisableControls;
    try
      Items.BeginUpdate;
      Items.Clear;
      FDataLink.DataSet.First;
      while not FDataLink.DataSet.Eof do
      begin
        aItem := Items.Add;
        with aItem do
        begin

          if (DataBinding.CaptionTemplate <> '') then
            Caption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, aItem.Index, FOnGetCaptionTemplateData)
          else if (DataBinding.CaptionField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
            if Assigned(aField) then
              Caption := aField.Text;
          end;

          if (DataBinding.InfoField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.InfoField);
            if Assigned(aField) then
              Info := aField.Text;
          end;

          if (DataBinding.NotesTemplate <> '') then
            Notes := HTMLDBReplace(DataBinding.NotesTemplate, FDataLink.DataSet, aItem.Index, FOnGetNotesTemplateData)
          else if (DataBinding.NotesField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.NotesField);
            if Assigned(aField) then
              Notes := aField.Text;
          end;

          if (DataBinding.CheckedField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
            if Assigned(aField) and (aField.DataType = ftBoolean) then
              Checked := aField.AsBoolean;
          end;

          if (DataBinding.HintField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
            if Assigned(aField) then
              Hint := aField.Text;
          end;

          if (DataBinding.ProgressValueField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.ProgressValueField);
            if Assigned(aField) then
            begin
              if (aField.DataType = ftInteger) then
                ProgressValue := aField.AsInteger
              else if (aField.DataType = ftFloat) then
                ProgressValue := aField.AsFloat;
            end;
          end;

          if (DataBinding.GraphicLeftField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicLeftField);
            if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
            begin
              blobf := aField as TBlobField;
              s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
              P := GraphicLeft.OnChange;
              GraphicLeft.OnChange := nil;
              try
                s.Position := 0;
                GraphicLeft.LoadFromStream(s);
                GraphicLeftType := gtImage;
              finally
                GraphicLeft.OnChange := P;
                s.Free;
              end;
            end;
          end;

          if (DataBinding.GraphicRightField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicRightField);
            if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
            begin
              blobf := aField as TBlobField;
              s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
              P := GraphicRight.OnChange;
              GraphicRight.OnChange := nil;
              try
                s.Position := 0;
                GraphicRight.LoadFromStream(s);
                GraphicRightType := gtImage;
              finally
                GraphicRight.OnChange := P;
                s.Free;
              end;
            end;
          end;

        end;

        if Assigned(OnDataBind) then
          OnDataBind(Self, aItem, FDataLink.DataSet.Fields);
        FDataLink.DataSet.Next;
      end;
    finally
      Items.EndUpdate;
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.LookupBarVisible: Boolean;
begin
  Result := inherited LookupBarVisible and not PageMode;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.Reload;
begin
  if not PageMode then
    LoadFromDataSet;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  Exit;

  if FDataLink.Edit then inherited
  else
  begin
    SetFocus;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.EditingChanged;
begin

end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.GetDataSource: TDataSource;
begin
  if not (csDestroying in ComponentState) and Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.GetRecordCount: Integer;
begin
  Result := 0;
  if CheckDataSet then
  begin
    Result := FDataLink.DataSet.RecordCount;
    if Assigned(FOnGetRecordCount) then
    begin
      FOnGetRecordCount(self, Result);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.GetSelectedIndex: Integer;
begin
  Result := -1;
  if Assigned(Items.SelectedItem) then
    Result := Items.SelectedItem.Index;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.InitPreview;
begin
  Items.Clear;
  Footer.Caption := 'Footer';
  Footer.Font.Size := 10;
  Footer.Font.Color := clWhite;

  Header.Caption := 'Header';
  Header.Font.Size := 10;
  Header.Font.Color := clWhite;

  //--- No default items creation
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN] then
    if not FDataLink.Edit then Key := 0;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.OnDataBindingChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and CheckDataSet then
  begin
    if (FDataBinding.GraphicLeftField <> '') and (DefaultItem.GraphicLeftType = gtNone) then
      DefaultItem.GraphicLeftType := gtImage;

    if (FDataBinding.GraphicRightField <> '') and (DefaultItem.GraphicRightType = gtNone) then
      DefaultItem.GraphicRightType := gtImage;
  end;

  if not (csLoading in ComponentState) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(FDataLink) and PageMode then
    if Assigned(FDataLink.DataSet) then
      if FDataLink.DataSet.Active then
      begin
        FDataLink.BufferCount := GetVisibleItemCount + 1;
      end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetDataBinding(const Value: TListBoxDataBinding);
begin
  FDataBinding.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.SetMultiSelect(const Value: Boolean);
begin
  inherited SetMultiSelect(False);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothListBox.UpdateItemCount;
begin
  if CheckDataSet and PageMode then
  begin
    ItemCount := GetRecordCount;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

{ TListBoxDataBinding }

procedure TListBoxDataBinding.Assign(Source: TPersistent);
begin
  if (Source is TListBoxDataBinding) then
  begin
    CaptionField := (Source as TListBoxDataBinding).CaptionField;
    CaptionTemplate := (Source as TListBoxDataBinding).CaptionTemplate;
    InfoField := (Source as TListBoxDataBinding).InfoField;
    NotesField := (Source as TListBoxDataBinding).NotesField;
    NotesTemplate := (Source as TListBoxDataBinding).NotesTemplate;
    GraphicLeftField := (Source as TListBoxDataBinding).GraphicLeftField;
    GraphicRightField := (Source as TListBoxDataBinding).GraphicRightField;
    CheckedField := (Source as TListBoxDataBinding).CheckedField;
    HintField := (Source as TListBoxDataBinding).HintField;
    ProgressValueField := (Source as TListBoxDataBinding).ProgressValueField;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TListBoxDataBinding.Create(AOwner: TDBAdvSmoothListBox);
begin
  inherited Create;
  FOwner := AOwner;
  FCaptionField := '';
  FCaptionTemplate := '';
  FInfoField := '';
  FNotesField := '';
  FNotesTemplate := '';
  FGraphicLeftField := '';
  FGraphicRightField := '';
  FCheckedField := '';
  FHintField := '';
  FProgressValueField := '';
end;

//------------------------------------------------------------------------------

destructor TListBoxDataBinding.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TListBoxDataBinding.GetDBListBox: TDBAdvSmoothListBox;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetCaptionField(const Value: String);
begin
  if (FCaptionField <> Value) then
  begin
    FCaptionField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetCaptionTemplate(const Value: String);
begin
  if (FCaptionTemplate <> Value) then
  begin
    FCaptionTemplate := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetCheckedField(const Value: string);
begin
  if (FCheckedField <> Value) then
  begin
    FCheckedField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetGraphicLeftField(const Value: string);
begin
  if (FGraphicLeftField <> Value) then
  begin
    FGraphicLeftField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetGraphicRightField(const Value: string);
begin
  if (FGraphicRightField <> Value) then
  begin
    FGraphicRightField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetHintField(const Value: string);
begin
  if (FHintField <> Value) then
  begin
    FHintField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetInfoField(const Value: string);
begin
  if (FInfoField <> Value) then
  begin
    FInfoField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetKeyField(const Value: string);
begin
  if (FKeyField <> Value) then
  begin
    FKeyField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetNotesField(const Value: string);
begin
  if (FNotesField <> Value) then
  begin
    FNotesField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetNotesTemplate(const Value: string);
begin
  if (FNotesTemplate <> Value) then
  begin
    FNotesTemplate := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TListBoxDataBinding.SetProgressValueField(const Value: string);
begin
  if (FProgressValueField <> Value) then
  begin
    FProgressValueField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

end.
