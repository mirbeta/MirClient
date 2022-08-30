{*************************************************************************}
{ TMS TAdvDBFilterPanel component                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2014 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvDBFilterPanel;

interface

uses
  Classes, Windows, StdCtrls, ExtCtrls, Controls, SysUtils, Dialogs, Forms,
  AdvGrid, ComCtrls, Buttons, AdvUtil, Spin, Graphics, DBGrids, Generics.Collections,
  DB, AdvCustomFilterPanel, GDIPicture;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : ColumnName property added to select between FieldName or DisplayLabel in Column
  // v1.0.1.1 : Fixed : Issue with floating point field values handling
  // v1.0.2.0 : New : ResetFilter method added to TAdvDBFilterPanel / TAdvDBFilterDialog
  // v1.0.3.0 : New : SortColumNames property added
  //          : New : DefaultOperation added
  //          : New : DefaultColumn added
  // v1.0.3.1 : Fixed : Issue with handling UI settings in dialog
  // v1.0.3.2 : Fixed : Issue with TAdvDBFilterDialog initialization
  // v1.0.3.3 : Improved: FilterDialog now shows filter data on show.

type
  TAdvDBFilterPanel = class;

  TFilterOperation = (foEqual, foNotEqual, foLargerThen, foSmallerThen, foContains, foEndsWith, foBeginsWith, foLargerOrEqual, foSmallerOrEqual, foTrueFalse );

  TFilterOperationItem = class(TCollectionItem)
  private
    FAction: string;
    FCase: Boolean;
    FColumn: string;
    FOperation: TFilterOperation;
    FOperator: string;
    FValue: string;
  published
    property Action: string read FAction write FAction;
    property CaseSensitive: Boolean read FCase write FCase;
    property Column: string read FColumn write FColumn;
    property Operation: TFilterOperation read FOperation write FOperation;
    property Symbol: string read FOperator write FOperator;
    property Value: string read FValue write FValue;
  end;

  TFilterOperations = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TFilterOperationItem;
    procedure SetItem(Index: Integer; const Value: TFilterOperationItem);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add: TFilterOperationItem;
    function Insert(Index: Integer): TFilterOperationItem;
    property Items[Index: Integer]: TFilterOperationItem read GetItem write SetItem;
  published
  end;

  TAdvFilterPanelDataLink = class(TDataLink)
  private
    FOnActiveChanged: TNotifyEvent;
  public
    procedure ActiveChanged; override;
  published
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
  end;

  TAdvFilterForm = class(TAdvCustomFilterForm)
  public
    function CreateFilterPanel(AOwner: TComponent): TAdvCustomFilterPanel; override;
  end;

  TFilterPanelFilterEvent = procedure(Sender: TObject; var AFilter: string) of object;

  TFilterColumnName = (cnFieldName, cnDisplayLabel);

  TAdvDBFilterPanel = class(TAdvCustomFilterPanel)
  private
    FDataLink: TAdvFilterPanelDataLink;
    FDataSource: TDataSource;
    FBeforeFilter: TFilterPanelFilterEvent;
    FAfterFilter:  TFilterPanelFilterEvent;
    FQueryString: string;
    FQueryParams: TFilterOperations;
    FInternalActivate: boolean;
    FColumnName: TFilterColumnName;
    procedure DataSetActiveChanged(Sender: TObject);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetQueryParams(const Value: TFilterOperations);
  protected
    procedure AddClear(Sender: TObject);  override;
    procedure AddFilterClick(Sender: TObject); override;
    procedure AddClickHandler(Sender: TObject); override;
    procedure DoAppliedFilter(var AFilter: string); virtual;
    procedure DoApplyFilter(var AFilter: string); virtual;
    procedure FilterToPanel(ADS: TDataSource);
    function GetColumnNames: TStringList; override;
    function GetFilterOperations: TStringList; override;
    function GetEditorType(AColumn: integer): TColumnEditor; override;
    procedure PanelToFilter(ADS: TDataSource);
    procedure RemoveClickHandler(Sender: TObject); override;
    procedure RestoreFilterClick(Sender: TObject);  override;
    procedure Init; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetFilter;
    property QueryParams: TFilterOperations read FQueryParams write SetQueryParams;
    property QueryString: String read FQueryString write FQueryString;
  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property ColumnName: TFilterColumnName read FColumnName write FColumnName default cnFieldName;
    property OnAfterFilter: TFilterPanelFilterEvent read FAfterFilter write FAfterFilter;
    property OnBeforeFilter: TFilterPanelFilterEvent read FBeforeFilter write FBeforeFilter;
  end;

  TAdvDBFilterDialog = class(TAdvCustomGridFilterDialog)
  private
    FDataSource: TDataSource;
    FBeforeFilter: TFilterPanelFilterEvent;
    FAfterFilter: TFilterPanelFilterEvent;
    FColumnName: TFilterColumnName;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetDataSource(const Value: TDataSource);
  protected
    function CreateFilterForm: TAdvCustomFilterForm; override;
    procedure FormShow(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoBeforeFilter(Sender: TObject; var AQuery: string);
    procedure DoAfterFilter(Sender: TObject; var AQuery: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetFilter;
  published
    property ColumnName: TFilterColumnName read FColumnName write FColumnName default cnFieldName;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property Version: string read GetVersion;
    property OnAfterFilter: TFilterPanelFilterEvent read FAfterFilter write FAfterFilter;
    property OnBeforeFilter: TFilterPanelFilterEvent read FBeforeFilter write FBeforeFilter;
  end;


implementation

uses
  AdvFilterPanelButton, TypInfo, RTTI;

const
  ERRGRIDASSIGN = 'Assign a TDataSource first to TAdvDBFilterPanel.DataSource';
  containerX = 5;
  containerY = 85;


function UPos(SubStr,Str: string; var APos: integer): integer;
begin
  APos := Pos(AnsiUppercase(SubStr),AnsiUppercase(Str));
  Result := APos;
end;

{ TAdvDBFilterPanel }

constructor TAdvDBFilterPanel.Create(AOwner: TComponent);
var
  lbl: TLabel;
begin
  inherited;

  FQueryParams := TFilterOperations.Create(Self);
  FQueryString := '';

  FColumnName := cnFieldName;

  FDataLink := TAdvFilterPanelDataLink.Create;
  FDataLink.OnActiveChanged := DataSetActiveChanged;

  lbl := TLabel.Create(Self);
  lbl.Caption := ERRGRIDASSIGN;
  lbl.Parent := CustomControl;
  lbl.Left := 20;
  lbl.Top := 20;

end;

procedure TAdvDBFilterPanel.DataSetActiveChanged(Sender: TObject);
var
  ctx : TRttiContext;
  t : TRttiType;
  Prop: TRttiProperty;
  v: TValue;
  s: TStrings;
begin
  if FInternalActivate then
    Exit;

  Init;
  ctx := TRttiContext.Create;
  try
    t := ctx.GetType(DataSource.DataSet.ClassInfo);
    for Prop in t.GetProperties do
    begin
      if Prop.Name = 'SQL' then
      begin
        v := Prop.GetValue(DataSource.DataSet);
        s := TStrings(v.AsObject);
        QueryString := s.GetText;
        Break;
      end;
    end;
  finally
    ctx.Free;
  end;

end;

destructor TAdvDBFilterPanel.Destroy;
begin
  FDataLink.Free;
  FQueryParams.Free;
  inherited;
end;

procedure TAdvDBFilterPanel.DoApplyFilter(var AFilter: string);
begin
  if Assigned(FBeforeFilter) then
    FBeforeFilter(Self, AFilter);
end;

procedure TAdvDBFilterPanel.DoAppliedFilter(var AFilter: string);
begin
  if Assigned(FAfterFilter) then
    FAfterFilter(Self, AFilter);
end;

procedure TAdvDBFilterPanel.AddClear(Sender: TObject);
var
  I: Integer;
begin
  inherited;

  if not Assigned(DataSource) then
    raise Exception.Create(ERRGRIDASSIGN);

  //free the box first
  if CustomControl.ControlCount - 1 >= 0 then
  begin
    for I := CustomControl.ControlCount - 1 downto 0 do
    begin
      CustomControl.Controls[I].Free;
    end;
  end;

  AddNewRow(0, false);
end;

procedure TAdvDBFilterPanel.AddClickHandler(Sender: TObject);
var
  ctrlscnt, ccount, ctrlpos: Integer;
  grpName: string;
  grp: TAdvFilterGroupBox;
  I, J: Integer;
begin
  ctrlpos := -1;
  ccount := -1; // combo counts

  // look for n° of controls in the scrollbox
  ctrlscnt := CustomControl.ControlCount;

  // find the group
  grpName := TAdvFilterGroupBox(TButton(Sender).Parent).Name;
  grp := nil;

  while ctrlpos = -1 do
  begin
    for I := 0 to CustomControl.ControlCount - 1 do
    begin
      if (CustomControl.Controls[I] is TAdvFilterGroupBox)  then
      begin
        if grpName = TAdvFilterGroupBox(CustomControl.Controls[I]).Name then
        begin
          grp := TAdvFilterGroupBox(CustomControl.Controls[I]);
          ctrlpos := I;
        end;
      end;
    end;
  end;

  ctrlpos := ctrlscnt - 1;

  //first move all other controls beneath us lower
  for J := 0 to CustomControl.ControlCount - 1 do
  begin
    if (CustomControl.Controls[J]).Top > grp.Top then
    begin
      (CustomControl.Controls[J]).Top :=  (CustomControl.Controls[J]).Top + containerY ;

      // lessen the count with every higher Y position
      if (ctrlpos - 1 >= 0) then
        Dec(ctrlpos);
    end;

    if (CustomControl.Controls[J] is TComboBox) then
    begin
      Inc(ccount);
    end;
  end;

  if (ctrlpos-ccount >= 0) and (ctrlpos = (ctrlscnt-1)) then
    ctrlpos := ctrlpos-ccount
  else
    if ctrlpos > 2 then
      ctrlpos := ctrlpos-1;

  if ctrlpos = 0 then
    AddNewRow(1,true)
  else
    AddNewRow(ctrlpos,true);

  SetCustomControlHeight;
end;

procedure TAdvDBFilterPanel.AddFilterClick(Sender: TObject);
begin
  inherited;

  if Assigned(FDataSource) then
  begin
    FilterToPanel(FDataSource);
  end
  else
    raise Exception.Create(ERRGRIDASSIGN);
end;

procedure TAdvDBFilterPanel.FilterToPanel(ADS: TDataSource);
var
  C: TCustomControl;
  combo: TComboBox;
  I,II,FO,VP: Integer;
  Con,Col,Query: string;
  Cs: Boolean;
  D: TFilterOperationItem;
  ctx : TRttiContext;
  t : TRttiType;
  Prop: TRttiProperty;
  hasSQL: boolean;
  cons: TStringList;
  fops: TStringList;
  PropInfo: PPropInfo;
  SQLObj: TStrings;

begin
  Query := '';
  CS := false;

  cons := TStringList.Create;
  fops := TStringList.Create;

  // loop over all Ces and actions
  for I := 0 to CustomControl.ControlCount - 1 do
  begin
    if CustomControl.Controls[I] is TCustomControl then
    begin
      D := FQueryParams.Add;

      C := TCustomControl(CustomControl.Controls[I]);

      // loop over all controls inside the C
      for II := 0 to C.ControlCount - 1 do
      begin
        // comboboxes
        if C.Controls[II] is TComboBox then
        begin
          // column filter
          if TComboBox(C.Controls[II]).Tag = tagColumn then
          begin
            combo := TComboBox(C.Controls[II]);

            if combo.ItemIndex >= 0 then
            begin
              if ColumnName = cnFieldName then
                Col := (combo.Items[combo.ItemIndex])+ ' '
              else
                Col := DataSource.DataSet.Fields[combo.ItemIndex].FieldName + ' ';

              D.Column := Col;
            end
            else
            begin
              Break;
            end;
          end;

          // operation filter
          if TComboBox(C.Controls[II]).Tag = tagOperation then
          begin
            FO := Integer(TComboBox(C.Controls[II]).Items.Objects[TComboBox(C.Controls[II]).ItemIndex]);

            if FO = Integer(foEqual) then
            begin
              Con := Con + Col + '= ''{SEARCH}''' ;
              D.Symbol := '=';
              D.Operation := foEqual;
            end;

            if FO = Integer(foNotEqual) then
            begin
              Con := Con + Col + '!= ''{SEARCH}''' ;
              D.Symbol := '!=';
              D.Operation := foNotEqual;
            end;

            if FO = Integer(foLargerThen) then
            begin
              Con := Con + Col + '> ''{SEARCH}''' ;
              D.Symbol :=  '>';
            end;

            if FO = Integer(foSmallerThen) then
            begin
              Con := Con + Col + '< ''{SEARCH}''' ;
              D.Symbol :=  '<';
            end;

            if FO = Integer(foContains) then
            begin
              Con := Con + Col + 'LIKE ''%{SEARCH}%''' ;
              D.Symbol := 'LIKE %{SEARCH}%';
            end;

            if FO = Integer(foEndsWith) then
            begin
              Con := Con + Col + 'LIKE ''%{SEARCH}''' ;
              D.Symbol := 'LIKE %{SEARCH}';
            end;

            if FO = Integer(foBeginsWith) then
            begin
              Con := Con + Col + 'LIKE ''{SEARCH}%''';
              D.Symbol := 'LIKE {SEARCH}%';
            end;

            if FO = Integer(foLargerOrEqual) then
            begin
              Con := Con + Col + '>= ''{SEARCH}''' ;
              D.Symbol := '>=';
            end;

            if FO = Integer(foSmallerOrEqual) then
            begin
              Con := Con + Col + '<= ''{SEARCH}''' ;
              D.Symbol := '<=';
            end;

            if FO = Integer(foTrueFalse) then
            begin
              Con := Con + Col + '= ''{SEARCH}''' ;
              D.Symbol := '=';
            end;
          end;
        end;

        // edits
        if C.Controls[II] is TEdit then
        begin
          D.Value := TEdit(C.Controls[II]).Text;
          D.Value := StringReplace(D.Value, ' ', '', [rfReplaceAll]);
          Con := StringReplace(Con, '{SEARCH}', D.Value,[rfReplaceAll, rfIgnoreCase]);

          cons.Add(con);
        end;

        // datetimepickers
        if C.Controls[II] is TDateTimePicker then
        begin
          D.Value := DateToStr(TDateTimePicker(C.Controls[II]).Date);
          D.Value := StringReplace(D.Value, ' ', '', [rfReplaceAll]);
          Con := StringReplace(Con, '{SEARCH}',  D.Value ,[rfReplaceAll, rfIgnoreCase]);
          Con := StringReplace(Con, '''',  '' ,[rfReplaceAll, rfIgnoreCase]);

          cons.Add(con);
        end;

        // spinedits
        if C.Controls[II] is TSpinEdit then
        begin
          D.Value := TSpinEdit(C.Controls[II]).Text;
          D.Value := StringReplace(D.Value, ' ', '', [rfReplaceAll]);

          D.Value := StringReplace(D.Value, ',', '.', [rfReplaceAll]);

          Con := StringReplace(Con, '''{SEARCH}''',  '{SEARCH}' ,[rfReplaceAll, rfIgnoreCase]);
          Con := StringReplace(Con, '{SEARCH}', D.Value,[rfReplaceAll, rfIgnoreCase]);

          cons.Add(con);
        end;

        // checkboxes
        if C.Controls[II] is TCheckBox then
        begin
          if C.Controls[II].Tag = tagValue then
          begin
            if TCheckBox(C.Controls[II]).Checked then
              D.Value := 'True'
            else
              D.Value := 'False';
            Con := StringReplace(Con, '{SEARCH}', D.Value,[rfReplaceAll, rfIgnoreCase]);

            cons.Add(con);
          end
          else
          begin
            CS := (TCheckBox(C.Controls[II]).Checked);
            D.CaseSensitive := CS;
          end;
        end;
      end; // for
    end; // if

    if CustomControl.Controls[I] is TComboBox then
    begin
      if TComboBox(CustomControl.Controls[I]).Tag = tagAction then
      begin
        con := '';
        if TComboBox(CustomControl.Controls[I]).ItemIndex = 0 then
        begin
          fops.Add(' AND ');
        end
        else
        begin
          fops.Add(' OR ');
        end; // if
      end; // if
    end; // if
  end; // for

  ctx := TRttiContext.Create;
  try
    t := ctx.GetType(ADS.DataSet.ClassInfo);

    hasSQL := false;

    for Prop in t.GetProperties do
    begin
      if Prop.Name = 'SQL' then
        hasSQL := true;
    end;

    if hasSQL then
    begin
      PropInfo := GetPropInfo(ADS.DataSet, 'SQL', [tkClass]);
      if  Assigned(PropInfo) then
      begin
        SQLObj := TStrings(GetObjectProp(ADS.DataSet, PropInfo, TStrings));

        con := '';
        for i := 0 to cons.Count - 1 do
          begin
            con := con + cons[i];

            if i < cons.Count - 1 then
              con := con + fops[i];
          end;

        if UPos('WHERE',FQueryString, VP) > 0 then
          FQueryString := Copy(FQueryString, 0, VP - 1);

        if Con <> '' then
          FQueryString := FQueryString + ' WHERE ' + Con;

        FInternalActivate := true;

        DoApplyFilter(FQueryString);

        SQLObj.Text := FQueryString;

        ADS.DataSet.Active := true;
        ADS.DataSet.Filter := Con;

        FInternalActivate := false;
        DoAppliedFilter(FQueryString);
      end;
    end
    else
    begin
      con := '';
      if cons.Count > 0 then
        con := cons[0];

      for i := 1 to cons.Count - 1 do
      begin
        if (fops[i - 1] = ' AND ') then
        begin
          con := con + fops[i - 1];
          con := con + cons[i];
        end;
      end;

      for i := 1 to cons.Count - 1 do
      begin
        if (fops[i - 1] = ' OR ') then
        begin
          con := con + fops[i - 1];
          con := con + cons[i];
        end;
      end;

      for Prop in t.GetProperties do
      begin
        if Prop.Name = 'Filter' then
        begin
          DoApplyFilter(FQueryString);
          

          ADS.DataSet.Filter := Con;
          ADS.DataSet.Filtered := true;
		  if not CS then
            ADS.DataSet.FilterOptions := ADS.DataSet.FilterOptions - [foCaseInSensitive]
          else
            ADS.DataSet.FilterOptions := ADS.DataSet.FilterOptions + [foCaseInSensitive];

          DoAppliedFilter(FQueryString);
          Break;
        end;
      end;
    end;

{
      if Prop.Name = 'SQL' then
      begin
        if UPos('WHERE',FQueryString, VP) > 0 then
          FQueryString := Copy(FQueryString, 0, VP - 1);
        FQueryString := FQueryString + ' WHERE ' + Con;

        ShowMessage('*'+FQueryString+'*');

        Prop.SetValue(ADS.DataSet,FQueryString);

        //ADS.DataSet.Filter := Con;
        //ADS.DataSet.Filtered := true;
        ADS.DataSet.Active := true;

        if not CS then
          ADS.DataSet.FilterOptions := ADS.DataSet.FilterOptions - [foCaseInSensitive]
        else
          ADS.DataSet.FilterOptions := ADS.DataSet.FilterOptions - [];

        DoFireEvent;
        Exit;
      end;

    end;
}
  finally
    cons.Free;
    fops.Free;
    ctx.Free;
  end;

end;

function TAdvDBFilterPanel.GetColumnNames: TStringlist;
var
  sl: TStringList;
  I: Integer;
begin
  sl := TStringList.Create;
  sl.Duplicates := dupIgnore;

  if Assigned(DataSource.Dataset) then
  begin
    for i := 0 to DataSource.Dataset.FieldCount - 1 do
    begin
      if ColumnName = cnDisplayLabel then
      begin
        if DataSource.DataSet.Fields[I].DisplayLabel <> '' then
          sl.AddObject(DataSource.DataSet.Fields[I].DisplayLabel, TObject(I))
        else
          sl.AddObject(IntToStr(I), TObject(I));
      end
      else
      begin
        if DataSource.DataSet.Fields[I].FieldName <> '' then
          sl.AddObject(DataSource.DataSet.Fields[I].FieldName, TObject(I))
        else
          sl.AddObject(IntToStr(I), TObject(I));
      end;
    end;
  end;

  Result := sl;
end;

function TAdvDBFilterPanel.GetEditorType(AColumn: integer): TColumnEditor;
var
  T: TFieldType;
begin
  Result := ceText;

  T := FDataSource.DataSet.Fields[AColumn].DataType;

  case T of
    TFieldType.ftInteger, TFieldType.ftFloat, TFieldType.ftSmallint, TFieldType.ftBCD, TFieldType.ftAutoInc, TFieldType.ftLargeint,
    TFieldType.ftShortint, TFieldType.ftExtended, TFieldType.ftSingle:
    begin
      Result := ceNumeric;
    end;
    TFieldType.ftString, TFieldType.ftUnknown, TFieldType.ftWord, TFieldType.ftCurrency,
    TFieldType.ftBytes, TFieldType.ftByte, TFieldType.ftMemo, TFieldType.ftFmtMemo, TFieldType.ftFixedChar, TFieldType.ftWideString,
    TFieldType.ftFixedWideChar, TFieldType.ftWideMemo, TFieldType.ftLongWord:
    begin
      Result := ceText;
    end;
    TFieldType.ftDate, TFieldType.ftDateTime, TFieldType.ftTimeStamp, TFieldType.ftOraTimeStamp:
    begin
      Result := ceDate;
    end;
    TFieldType.ftTime:
    begin
      Result := ceTime;
    end;
    TFieldType.ftBoolean:
    begin
      Result := ceBoolean;
    end;
  end;

end;

function TAdvDBFilterPanel.GetFilterOperations: TStringList;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.AddObject(UI.OperationEqual, TObject(foEqual));
  sl.AddObject(UI.OperationNotEqual,TObject(foNotEqual));
  sl.AddObject(UI.OperationLargerThen,TObject(foLargerThen));
  sl.AddObject(UI.OperationSmallerThen,TObject(foSmallerThen));
  sl.AddObject(UI.OperationContains,TObject(foContains));
  sl.AddObject(UI.OperationEndsWith,TObject(foEndsWith));
  sl.AddObject(UI.OperationBeginsWith,TObject(foBeginsWith));
  sl.AddObject(UI.OperationLargerOrEqual,TObject(foLargerOrEqual));
  sl.AddObject(UI.OperationSmallerOrEqual,TObject(foSmallerOrEqual));
  sl.AddObject(UI.OperationTrueFalse,TObject(foTrueFalse));
  Result := sl;
end;

procedure TAdvDBFilterPanel.Init;
begin
  AddClear(Self);
end;

procedure TAdvDBFilterPanel.PanelToFilter(ADS: TDataSource);
var
  I,II,loopCount,VP: Integer;
  C: TCustomControl;
  combo, combo2: TComboBox;
  check: TCheckbox;
  cond_expr, fltr_expr,fltr_suff: string;
  celltype: TFieldType;
  F, Tmp: String;
  Aof: TStringList;
  Aoa: TStringList;
  FC: TFilterOperationItem;
  Y: Integer;
begin
  if not Assigned(ADS) then
    raise Exception.Create(ERRGRIDASSIGN);

  F := AnsiLowerCase(ADS.DataSet.Filter);
  F := StringReplace(F,'(','',[rfReplaceAll]);
  F := StringReplace(F,')','',[rfReplaceAll]);

  Aof := TStringList.Create;
  Aoa := TStringList.Create;

  try
    // Extract all filter conditions

    while (UPos(' AND ',F, VP) - 1 > 0) or (UPos(' OR ', F, VP) - 1 > 0) do
    begin
      if(UPos(' AND ', F, VP) - 1 > 0) then
      begin
        Tmp := Copy(F, 0, VP - 1);
        Aof.Add(Tmp);
        F := StringReplace(F, Tmp + ' and ', '', [rfReplaceAll]) + ' ';
        Aoa.Add('and');
      end;

      if (UPos(' OR ', F, VP) - 1 > 0) then
      begin
        Tmp := Copy(F, 0, VP - 1);
        Aof.Add(Tmp);
        F := StringReplace(F, Tmp + ' or ', '', [rfReplaceAll]) + ' ';
        Aoa.Add('or');
      end;
    end;

    Aof.Add(F);
    FQueryParams.Clear;

    I := 0;
    // Extracting
    while (I < Aof.Count) and (Aof[I] <> '')  do
    begin
      // Extract Field
      Tmp := Copy(Aof[I], 0, UPos(' ', Aof[I], VP) - 1);
      Aof[I] := StringReplace(Aof[I], Tmp, '',[]);
      FC := FQueryParams.Add;
      FC.Column := Tmp;

      // Remove trailing spaces
      if Copy(Aof[I], 0, 1) = ' ' then
      begin
        Aof[I] := StringReplace(Aof[I], ' ', '', []);
      end;

      // Extract Operator
      Tmp := Copy(Aof[I], 0, UPos(' ', Aof[I], VP) - 1);
      Aof[I] := StringReplace(Aof[I], Tmp, '',[]);
      FC.Symbol := Tmp;

      // Extract Value
      FC.Value := StringReplace(Aof[I], '''', '',[rfReplaceAll]);

      // Extract Action
      if Aoa.Count > I then
        FC.Action := Aoa[I];

      Inc(I);
    end;

    AddClear(Self);

    //first make all necessary rows
    for I := 1 to FQueryParams.Count -1 do
    begin
      AddNewRow(I, true);
    end;

    loopCount := -1;

    for I := 0 to CustomControl.ControlCount - 1 do
    begin
      if CustomControl.Controls[I] is TCustomControl then
      begin
        C := TCustomControl(CustomControl.Controls[I]);

        for II := 0 to C.ControlCount - 1 do
        begin
          if C.Controls[II] is TComboBox then
          begin
            combo2 := TComboBox(C.Controls[II]);

            if combo2.Tag = tagColumn then
            begin
              loopcount := loopcount + 1;

              if FQueryParams.Count > 0 then
              begin
                for Y := 0 to combo2.Items.Count - 1 do
                begin
                  if AnsiLowerCase(StringReplace(TFilterOperationItem(FQueryParams.Items[loopcount]).Column, ' ', '', [rfReplaceAll])) = AnsiLowerCase(combo2.Items[Y]) then
                  begin
                  combo2.ItemIndex := Y;
                  Tmp := IntToStr(combo2.ItemIndex);
                  end;
                end;
              end
              else
              begin
                break;
              end;
            end;//if tag = column

            cond_expr := TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol;

            if (combo2.Tag = tagOperation) and (cond_expr <> '') then
            begin
              fltr_expr := Copy(cond_expr,1,2);
              fltr_suff := Copy(cond_expr,Length(cond_expr), 1);

              if fltr_expr = '<=' then
              begin
                combo2.ItemIndex := Integer(foSmallerOrEqual);
                TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr,3,Length(cond_expr));
              end //if condition <=
              else
              if fltr_expr = '>=' then
              begin
                combo2.ItemIndex := Integer(foLargerOrEqual);
                TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol := Copy(cond_expr,3,Length(cond_expr));
              end //if condition <=
              else
              begin
                fltr_expr := Copy(cond_expr,1,1);

                if ((fltr_expr = '*') and (fltr_suff = '*')) or (cond_expr = 'like') then
                begin
                  combo2.ItemIndex := Integer(foContains);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol := Copy(cond_expr,2,Length(cond_expr) - 1);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol := Copy(TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol,1,Length(TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol)-1);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Value := StringReplace(TFilterOperationItem(FQueryParams.Items[loopcount]).Value, '%', '', [rfReplaceAll]);

                end //if condition *xxxx*
                else
                if fltr_expr = '=' then
                begin
                  if (TFilterOperationItem(FQueryParams.Items[loopcount]).Value = 'True') or (TFilterOperationItem(FQueryParams.Items[loopcount]).Value = 'False') then
                    combo2.ItemIndex := Integer(foTrueFalse)
                  else
                    combo2.ItemIndex := Integer(foEqual);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol := Copy(cond_expr,2,Length(cond_expr));
                end //if condition =
                else
                if fltr_expr = '<' then
                begin
                  combo2.ItemIndex := Integer(foSmallerThen);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr,2,Length(cond_expr));
                end //if condition <
                else
                if fltr_expr = '>' then
                begin
                  combo2.ItemIndex := Integer(foLargerThen);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr,2,Length(cond_expr));
                end//if condition >
                else
                if fltr_expr = '*' then
                begin
                  combo2.ItemIndex := Integer(foEndsWith);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr,2,Length(cond_expr));
                end //if condition xxx*
                else
                if fltr_suff = '*' then
                begin
                  combo2.ItemIndex := Integer(foBeginsWith);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr,1,Length(cond_expr) - 1);;
                end //if condition *xxx
                else
                if fltr_expr = '!' then
                begin
                  combo2.ItemIndex := Integer(foNotEqual);
                  TFilterOperationItem(FQueryParams.Items[loopcount]).Symbol:= Copy(cond_expr, 2, Length(cond_expr));
                end;//if condition !
              end;

              // loop over all controls inside the C to remove the TEdit
              for Y := C.ControlCount - 1 downto 0 do
              begin
                // comboboxes
                if (C.Controls[Y].Tag = tagValue) and not (C.Controls[Y] is TLabel)then
                begin
                   C.Controls[Y].Free;
                end;
              end;

              // make the value edit
              celltype := FDataSource.DataSet.Fields[StrToInt(Tmp)].DataType;

              case celltype of
                TFieldType.ftInteger, TFieldType.ftFloat, TFieldType.ftSmallint, TFieldType.ftBCD, TFieldType.ftAutoInc, TFieldType.ftLargeint,
                TFieldType.ftShortint, TFieldType.ftExtended, TFieldType.ftSingle:
                begin
                  AddSpinEdit(C, TFilterOperationItem(FQueryParams.Items[loopcount]).Value);
                end;
                TFieldType.ftString, TFieldType.ftUnknown, TFieldType.ftWord, TFieldType.ftCurrency,
                TFieldType.ftBytes, TFieldType.ftByte, TFieldType.ftMemo, TFieldType.ftFmtMemo, TFieldType.ftFixedChar, TFieldType.ftWideString,
                TFieldType.ftFixedWideChar, TFieldType.ftWideMemo, TFieldType.ftLongWord:
                begin
                  AddEdit(C, loopcount, TFilterOperationItem(FQueryParams.Items[loopcount]).Value);
                end;
                TFieldType.ftDate, TFieldType.ftDateTime, TFieldType.ftTimeStamp, TFieldType.ftOraTimeStamp:
                begin
                  AddDate(C, TFilterOperationItem(FQueryParams.Items[loopcount]).Value);
                end;
                TFieldType.ftTime:
                begin
                  AddDate(C, TFilterOperationItem(FQueryParams.Items[loopcount]).Value);
                end;
                TFieldType.ftBoolean:
                begin
                  AddBool(C, StrToBool(TFilterOperationItem(FQueryParams.Items[loopCount]).Value));
                end;
              end;
            end;//if tag = operation
          end;//if TComboBox

          if C.Controls[II] is TCheckBox then
          begin
            check := TCheckBox(C.Controls[II]);
            if check.Tag = tagCase then
            begin
              check.Checked := TFilterOperationItem(FQueryParams.Items[loopcount]).CaseSensitive;
            end;
          end;//if checkbox

        end;//for C
      end;//if C

      if CustomControl.Controls[I] is TComboBox then
      begin
        combo := TComboBox(CustomControl.Controls[I]);
        if TFilterOperationItem(FQueryParams.Items[loopcount]).Action = 'and' then
        begin
          combo.ItemIndex := 0;
        end
        else
        begin
          combo.ItemIndex := 1;
        end;//if else operation
      end;//if ComboBox
    end;//for scrollbox
  finally
    Aoa.Free;
    Aof.Free;
  end;
end;

procedure TAdvDBFilterPanel.RemoveClickHandler(Sender: TObject);
var
  C: Integer;
  container: TAdvFilterGroupBox;
  I: Integer;
  Y: single;
  G: TControl;
  sl: TStringList;
begin
  // look for n° of controls in the scrollbox
  C := CustomControl.ControlCount;
  sl := TStringList.Create;

  container := TAdvFilterGroupBox(TButton(Sender).Parent);
  Y := container.Top;

  if C = 1 then
  begin
    CustomControl.Controls[0].Free;

    sl.Free;
    Exit;
  end;

  if C > 1 then
  begin
    if Y < containerY then
    begin
      CustomControl.Controls[1].Free;
      CustomControl.Controls[0].Free;

      //Lower the positions
      for I := 0 to C - 3 do
      begin
        G := CustomControl.Controls[I];
        G.Top := G.Top - containerY;
      end;

      sl.Free;

      Exit;
    end;

    I := C - 1;
    while (I > 0) do
    begin
      if Assigned(CustomControl.Controls[I]) then
      begin
        G := CustomControl.Controls[I];

        // All above => lower
        if G.Top > Y then
        begin
          G.Top := G.Top - containerY;
        end
        else
        begin
          if G.Top = Y then
          begin
            sl.Add(intToStr(I));
            sl.Add(intToStr(I-1));
            Dec(I);
          end;
        end;
      end;
      Dec(I);
    end;
  end;

  I := strToInt(sl[0]);
  CustomControl.Controls[I].Free;
  I := strToInt(sl[1]);
  CustomControl.Controls[I].Free;

  sl.Free;
end;

procedure TAdvDBFilterPanel.ResetFilter;
begin
  AddClear(Self);
  FilterToPanel(FDataSource);
end;

procedure TAdvDBFilterPanel.RestoreFilterClick(Sender: TObject);
begin
  inherited;
  PanelToFilter(FDataSource);
end;

procedure TAdvDBFilterPanel.SetDataSource(const Value: TDataSource);
var
  I: Integer;
  lbl: TLabel;
begin
  if Assigned(Value) then
  begin
    FDataSource := Value;
    FDataLink.DataSource := Value;
    AddClear(Self);
  end
  else
  begin
    //free the box first
    if CustomControl.ControlCount - 1 >= 0 then
    begin
      for I := CustomControl.ControlCount - 1 downto 0 do
      begin
        CustomControl.Controls[I].Free;
      end;
    end;

    lbl := TLabel.Create(Self);
    lbl.Caption := ERRGRIDASSIGN;
    lbl.Parent := CustomControl;
  end;
end;

procedure TAdvDBFilterPanel.SetQueryParams(const Value: TFilterOperations);
begin
  FQueryParams.Assign(Value);
end;

{ TAdvDBFilterDialog }

constructor TAdvDBFilterDialog.Create(AOwner: TComponent);
begin
  inherited;
  FColumnName := cnFieldName;

end;

function TAdvDBFilterDialog.CreateFilterForm: TAdvCustomFilterForm;
begin
  Result := TAdvFilterForm.CreateNew(Application);

  ((Result as TAdvFilterForm).FilterPanel as TAdvDBFilterPanel).ColumnName := FColumnName;
  ((Result as TAdvFilterForm).FilterPanel as TAdvDBFilterPanel).OnAfterFilter := DoAfterFilter;
  ((Result as TAdvFilterForm).FilterPanel as TAdvDBFilterPanel).OnBeforeFilter := DoBeforeFilter;
end;

destructor TAdvDBFilterDialog.Destroy;
begin
  inherited;
end;

procedure TAdvDBFilterDialog.DoAfterFilter(Sender: TObject; var AQuery: string);
begin
  if Assigned(OnAfterFilter) then
    OnAfterFilter(Sender, AQuery);
end;

procedure TAdvDBFilterDialog.DoBeforeFilter(Sender: TObject;
  var AQuery: string);
begin
  if Assigned(OnBeforeFilter) then
    OnBeforeFilter(Sender, AQuery);
end;

procedure TAdvDBFilterDialog.FormShow(Sender: TObject);
begin
  inherited;
  ((Form as TAdvFilterForm).FilterPanel as TAdvDBFilterPanel).DataSource := FDataSource;
  ((Form as TAdvFilterForm).FilterPanel as TAdvDBFilterPanel).PanelToFilter(FDataSource);
end;

function TAdvDBFilterDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvDBFilterDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvDBFilterDialog.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TAdvDBFilterDialog.ResetFilter;
begin
  ((Form as TAdvCustomFilterForm).FilterPanel as TAdvDBFilterPanel).ResetFilter;
end;

procedure TAdvDBFilterDialog.SetDataSource(const Value: TDataSource);
begin
  FDataSource := Value;
end;

{ TFilterCollection }

function TFilterOperations.Add: TFilterOperationItem;
begin
  Result := TFilterOperationItem(inherited Add);
end;

constructor TFilterOperations.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TFilterOperationItem);
end;

destructor TFilterOperations.Destroy;
begin
  inherited;
end;

function TFilterOperations.GetItem(Index: Integer): TFilterOperationItem;
begin
  Result := TFilterOperationItem(inherited Items[Index]);
end;

function TFilterOperations.Insert(Index: Integer): TFilterOperationItem;
begin
  Result := TFilterOperationItem(inherited Insert(Index));
end;

procedure TFilterOperations.SetItem(Index: Integer; const Value: TFilterOperationItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvFilterPanelDataLink }

procedure TAdvFilterPanelDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(FOnActiveChanged) then FOnActiveChanged(Self);
end;

{ TAdvFilterForm }

function TAdvFilterForm.CreateFilterPanel(AOwner: TComponent): TAdvCustomFilterPanel;
begin
  Result := TAdvDBFilterPanel.Create(AOwner);
end;


end.
