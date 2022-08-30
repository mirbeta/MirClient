{*************************************************************************}
{ TDBTodoList component                                                   }
{ for Delphi  & C++Builder                                                }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001-2012                                         }
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

unit DBTodoList;

interface

{$I TMSDEFS.INC}
uses
  DB, TodoList, Classes, ActiveX, ComObj, Windows, SysUtils;

//{$DEFINE USE_PLANNERDATEPICKER}  

type
  TDBTodoList = class;

  TCreateDBKeyEvent = procedure(Sender: TObject; ATodoItem: TTodoItem; var DBKey: string) of object;

  TTodoFields = class(TPersistent)
  private
    FDBTodoList: TDBTodoList;
    FNotesField: string;
    FCompletionDateField: string;
    FSubjectField: string;
    FDueDateField: string;
    FCreationDateField: string;
    FResourceField: string;
    FCompleteField: string;
    FStatusField: string;
    FImageField: string;
    FPriorityField: string;
    FKeyField: string;
    FCompletionField: string;
    FProjectField: string;
    FCategoryField: string;
    FAutoIncKey: Boolean;
    FTotalTimeField: String;
  protected
  public
    constructor Create( AOwner: TDBTodoList);
    property DBTodoList: TDBTodoList read FDBTodoList;
  published
    property AutoIncKey: Boolean read FAutoIncKey write FAutoIncKey; 
    property SubjectField: string read FSubjectField write FSubjectField;
    property NotesField: string read FNotesField write FNotesField;
    property CompleteField: string read FCompleteField write FCompleteField;
    property DueDateField: string read FDueDateField write FDueDateField;
    property CategoryField: string read FCategoryField write FCategoryField;
    property CreationDateField: string read FCreationDateField write FCreationDateField;
    property CompletionField: string read FCompletionField write FCompletionField;
    property CompletionDateField: string read FCompletionDateField write FCompletionDateField;
    property ResourceField: string read FResourceField write FResourceField;
    property PriorityField: string read FPriorityField write FPriorityField;
    property StatusField: string read FStatusField write FStatusField;
    property ImageField: string read FImageField write FImageField;
    property KeyField: string read FKeyField write FKeyField;
    property ProjectField: string read FProjectField write FProjectField;
    property TotalTimeField: string read FTotalTimeField write FTotalTimeField;
  end;

  TDBTodoListDataLink = class(TDataLink)
  private
    FDBTodoList: TDBTodoList;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADBTodoList: TDBTodoList);
    destructor Destroy; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBTodoList = class(TCustomTodoList)
  private
    FDataLink: TDBTodoListDataLink;
    FTodoFields: TTodoFields;
    FDBUpdating: Boolean;
    FOnCreateDBKey: TCreateDBKeyEvent;
    FClearListOnImport: Boolean;
    procedure SetTodoFields(const Value: TTodoFields);
    procedure SetDataSource(const Value: TDataSource);
    function GetDataSource: TDataSource;
    procedure InputFromCSV(FileName: string; insertmode: Boolean);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SyncItems;
    procedure SyncRecord;
    procedure SyncCursor;
    procedure DBInsert(ATodoItem: TTodoItem);
    procedure DBDelete(ATodoItem: TTodoItem);
    procedure DBUpdate(ATodoItem: TTodoItem);
    procedure DBWrite(D: TDataSet; ATodoItem: TTodoItem);
    procedure DBRead(D: TDataSet; ATodoItem: TTodoItem);
    procedure EditDone(Data: TTodoData; EditItem: TTodoItem); override;
    function AllowAutoDelete(ATodoItem: TTodoItem): Boolean; override;
    function AllowAutoInsert(ATodoItem: TTodoItem): Boolean; override;
    procedure ItemSelect(ATodoItem: TTodoItem); override;
    function CheckDataSet: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem: TTodoItem;
    procedure FreeItem(ATodoItem: TTodoItem);
    procedure UpdateItem(ATodoItem: TTodoItem);
    procedure ImportFromCSV(FileName: string); override;
    procedure Reload;
    property ClearListOnImport : Boolean Read fClearListOnImport Write fClearListOnImport;
  published
    property ActiveColumnColor;
    property ActiveItemColor;
    property ActiveItemColorTo;
    property Align;
    property Anchors;
    property AutoAdvanceEdit;
    property AutoInsertItem;
    property AutoDeleteItem;
    property AutoThemeAdapt;
    property BorderStyle;
{$IFDEF USE_PLANNERDATEPICKER}
    property CalendarType;
{$ENDIF}
    property Category;
    property Color;
    property Columns;
    property CompleteCheck;
    property CompletionFont;
    property CompletionGraphic;
    property Cursor;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DateFormat;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Editable;
    property EditColors;
    property EditSelectAll;
    property Font;
    property GridLines;
    property GridLineColor;
    property HandleGlyph;
    property HeaderActiveColor;
    property HeaderActiveColorTo;    
    property HeaderColor;
    property HeaderColorTo;    
    property HeaderDragDrop;
    property HeaderFont;
    property HeaderHeight;
    property HeaderImages;
    property HintShowFullText;
    property Images;
    property ItemHeight;
    property NullDate;
    property PopupMenu;
    property Preview;
    property PreviewFont;
    property PreviewHeight;
    property PreviewColor;
    property PreviewColorTo;        
    property PriorityFont;
    property PriorityStrings;
    property PriorityListWidth;
    property ProgressLook;    
    property SelectionColor;
    property SelectionColorTo;
    property SelectionFontColor;
    property ShowPriorityText;
    property ShowSelection;
    property Sorted;
    property SortDirection;
    property SortColumn;
    property StatusStrings;
    property StretchLastColumn;    
    property TabOrder;
    property TabStop;
    property TodoFields: TTodoFields read FTodoFields write SetTodoFields;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditDone;
    property OnEditStart;
    property OnEnter;
    property OnExit;
    property OnHeaderClick;
    property OnHeaderRightClick;
    property OnItemDelete;
    property OnItemInsert;
    property OnItemSelect;
    property OnItemRightClick;
    property OnStartDrag;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnStatusToString;
    property OnStringToStatus;
    property OnPriorityToString;
    property OnStringToPriority;
    property OnCreateDBKey: TCreateDBKeyEvent read FOnCreateDBKey write FOnCreateDBKey;
    property OnCompleteClick;
    property Version;
  end;


implementation

{ TDBTodoList }

constructor TDBTodoList.Create(AOwner: TComponent);
begin
  inherited;
  FTodoFields := TTodoFields.Create(Self);
  FDataLink := TDBTodoListDataLink.Create(Self);
  FClearListOnImport := True;
end;

destructor TDBTodoList.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FTodoFields.Free;
  FTodoFields := nil;
  inherited;
end;

procedure TDBTodoList.SetTodoFields(const Value: TTodoFields);
begin
  FTodoFields.Assign(Value);
end;

procedure TDBTodoList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = DataSource) then
  begin
    DataSource := nil;
    Items.Clear;
  end;
end;

function TDBTodoList.CheckDataSet: Boolean;
begin
  Result := True;
  if not Assigned(FDataLink) then
    Result := False
  else
    if not Assigned(DataSource) then
      Result := False
    else
      if not Assigned(DataSource.DataSet) then
        Result := False
      else
        if not DataSource.DataSet.Active then
          Result := False;
end;


procedure TDBTodoList.SyncItems;
var
  B: TBookMark;
  D: TDataSet;
  ATodoItem: TTodoItem;
  
begin
  Items.Clear;

  if not CheckDataSet then
    Exit
  else
  begin
    D := DataSource.DataSet;

    D.DisableControls;

    B := D.GetBookMark;

    FDBUpdating := True;

    D.First;

    while not D.Eof do
    begin
      ATodoItem := Items.Add;

      DBRead(D,ATodoItem);

      D.Next;
    end;

    D.GotoBookmark(B);

    FDBUpdating := False;    

    D.EnableControls;

    D.FreeBookMark(B);
  end;
end;

procedure TDBTodoList.SetDataSource(const Value: TDataSource);
begin
  if Assigned(FDataLink) then
    if FDataLink.DataSource <> Value then
    begin
      FDataLink.DataSource := Value;
    end;
end;

function TDBTodoList.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TDBTodoList.InputFromCSV(FileName: string; insertmode: Boolean);
var
  buffer, celltext: string;
  s: Integer;
  f: TextFile;
  delimiterpos, quotepos: Integer;
  lr: TStringList;
  ColCount, dif: integer;
  newItem: TTodoItem;
  iItem: Integer;
begin

  if FClearListOnImport then
  begin
    for iItem := (Items.Count - 1) downto 0 Do
    begin
      FreeItem(Items[iItem]);
    end;
  end;

  ColCount := 15;

  dif := 1;
  AssignFile(f, FileName);
{$I-}
  Reset(f);
{$I+}
  if (IOResult <> 0) then
    raise Exception.Create('Cannot open file ' + FileName);


  lr := TStringList.Create;

  // do intelligent estimate of the separator
  Reset(f);

  while not Eof(f) do
  begin
    ReadLn(f, buffer);
    lr.CommaText := Buffer;

    s := 1; //StrtCol;

    while VarCharPos(#0 {FDelimiter}, Buffer, DelimiterPos) > 0 do
    begin
      if Buffer[1] = '"' then
      begin
        Delete(buffer, 1, 1); //delete first quote from buffer

        if SinglePos('"', Buffer, QuotePos) > 0 then //search for next single quote
        begin
          CellText := Copy(buffer, 1, QuotePos - 1);
          CellText := DoubleToSingleChar('"', CellText);
          Delete(buffer, 1, QuotePos);
        end
        else
          CellText := '';
        VarCharPos(#0 {FDelimiter}, buffer, DelimiterPos);
      end
      else
      begin
        CellText := Copy(buffer, 1, DelimiterPos - 1);
        CellText := DoubleToSingleChar('"', CellText);
      end;

     { if JavaCSV then
        JavaToLineFeeds(CellText)
      else  }
      CSVToLineFeeds(CellText);

      Delete(buffer, 1, DelimiterPos);

      Inc(s);
      if s >= ColCount then
        ColCount := s;
    end;

    if Length(Buffer) > 0 then
    begin
      if Buffer[1] = '"' then
        Delete(buffer, 1, 1);
      if Length(Buffer) > 0 then
      begin
        if Buffer[Length(Buffer)] = '"' then
          Delete(Buffer, Length(Buffer), 1);
      end;

      CellText := DoubleToSingleChar('"', Buffer);
     {
      if JavaCSV then
        JavaToLineFeeds(CellText)
      else  }
      CSVToLineFeeds(CellText);


      Inc(s);
      if s > ColCount then
        ColCount := s;
    end;

    newItem := CreateItem;
    newItem.Subject := lr[0];
    newItem.Resource := lr[1];
    newItem.Notes.text := lr[2]; //WriteToCSV(CellText);
    newItem.Status := StringToStatus(lr[2 + dif]);
    newItem.Project := lr[3 + dif];
    newItem.Category := lr[4 + dif];
    newItem.Complete := Lr[5 + dif] = 'True';
    newItem.Completion := StrtoInt(lr[6 + dif]);
    newItem.CompletionDate := StrToDate(lr[7 + dif]);
    newItem.CreationDate := StrToDate(lr[8 + dif]);
    newItem.DueDate := StrToDate(lr[9 + dif]);
    newItem.TotalTime := StrToFloat(lr[10 + dif]);
    newItem.Tag := Strtoint(lr[11 + dif]);
    newItem.ImageIndex := StrToInt(lr[12 + dif]);
    newItem.Priority := StringToPriority(lr[13 + dif]);
    DBUpdate(newItem);

  end;

  CloseFile(f);
  lr.Free;
//  UpdateHScrollExtent(0)
end;


procedure TDBTodoList.EditDone(Data: TTodoData; EditItem: TTodoItem);
var
  Field: TField;
  D: TDataSet;
begin
  inherited;

  if not CheckDataSet then
    Exit;

  if ((EditItem.DBKey <> '') or (FTodoFields.AutoIncKey)) and (FTodoFields.KeyField <> '') then
  begin
    D := FDataLink.DataSource.DataSet;
    Field := D.FieldByName(FTodoFields.KeyField);
    if Assigned(Field) then
    begin
      FDBUpdating := True;
      FDataLink.DataSource.DataSet.Locate(FTodoFields.KeyField, EditItem.DBKey,[]);

      D.Edit;

      case Data of
      tdSubject: if FTodoFields.SubjectField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.SubjectField);
          Field.AsString := EditItem.Subject;
        end;
      tdCategory: if FTodoFields.CategoryField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.CategoryField);
          Field.AsString := EditItem.Category;
        end;
      tdResource: if FTodoFields.ResourceField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.ResourceField);
          Field.AsString := EditItem.Resource;
        end;
      tdNotes: if FTodoFields.NotesField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.NotesField);
          Field.AsString := EditItem.Notes.Text;
        end;
      tdCreationDate:  if FTodoFields.CreationDateField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.CreationDateField);
          Field.AsDateTime := EditItem.CreationDate;
        end;
      tdCompletionDate: if FTodoFields.CompletionDateField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.CompletionDateField);
          Field.AsDateTime := EditItem.CompletionDate;
        end;
      tdDueDate: if FTodoFields.DueDateField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.DueDateField);
          Field.AsDateTime := EditItem.DueDate;
        end;
      tdComplete: if FTodoFields.CompleteField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.CompleteField);
          Field.AsBoolean := EditItem.Complete;
        end;
      tdCompletion: if FTodoFields.CompletionField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.CompletionField);
          Field.AsInteger := EditItem.Completion;
        end;
      tdStatus: if FTodoFields.StatusField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.StatusField);
          Field.AsInteger := Integer(EditItem.Status);
        end;
      tdPriority: if FTodoFields.PriorityField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.PriorityField);
          Field.AsInteger := Integer(EditItem.Priority);
        end;
      tdProject: if FTodoFields.ProjectField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.ProjectField);
          Field.AsString := EditItem.Project;
        end;
      tdImage: if FTodoFields.ImageField <> '' then
        begin
          Field := D.FieldByName(FTodoFields.ImageField);
          Field.AsInteger := EditItem.ImageIndex;
        end;
      tdTotalTime: if FTodoFields.TotalTimeField <> '' then
        begin
           Field := D.FieldByName(FTodoFields.TotalTimeField);
           Field.AsFloat := EditItem.TotalTime;
        end;
      end;

      D.Post;
      
      FDBUpdating := False;
    end;
  end;
end;

procedure TDBTodoList.DBDelete(ATodoItem: TTodoItem);
var
  D: TDataSet;
  Field: TField;
begin
  if CheckDataSet then
  begin
    if (ATodoItem.DBKey <> '') and (FTodoFields.KeyField <> '') then
    begin
      D := FDataLink.DataSource.DataSet;
      Field := D.FieldByName(FTodoFields.KeyField);
      if Assigned(Field) then
      begin
        FDBUpdating := True;
        FDataLink.DataSource.DataSet.Locate(FTodoFields.KeyField, ATodoItem.DBKey,[]);
        D.Delete;
        FDBUpdating := False;
      end;
    end;
  end;
end;

function TDBTodoList.AllowAutoDelete(ATodoItem: TTodoItem): Boolean;
begin
  Result := inherited AllowAutoDelete(ATodoItem);
  if Result then
    DBDelete(ATodoItem);
end;

function TDBTodoList.AllowAutoInsert(ATodoItem: TTodoItem): Boolean;
begin
  Result := inherited AllowAutoInsert(ATodoItem);
  if Result then
    DBInsert(ATodoItem);
end;

function TDBTodoList.CreateItem: TTodoItem;
begin
  Result := Items.Add;
  DBInsert(Result);
end;

procedure TDBTodoList.DBRead(D: TDataSet; ATodoItem: TTodoItem);
var
  Field: TField;
begin
  if FTodoFields.KeyField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.KeyField);
    if Assigned(Field) then
      ATodoItem.DBKey := Field.AsString;
  end;

  if FTodoFields.SubjectField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.SubjectField);
    if Assigned(Field) then
      ATodoItem.Subject := Field.AsString;
  end;

  if FTodoFields.CategoryField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CategoryField);
    if Assigned(Field) then
      ATodoItem.Category := Field.AsString;
  end;

  if FTodoFields.NotesField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.NotesField);
    if Assigned(Field) then
      ATodoItem.Notes.Text := Field.AsString;
  end;

  if FTodoFields.StatusField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.StatusField);
    if Assigned(Field) then
      ATodoItem.Status := TTodoStatus(Field.AsInteger);
  end;

  if FTodoFields.PriorityField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.PriorityField);
    if Assigned(Field) then
      ATodoItem.Priority := TTodoPriority(Field.AsInteger);
  end;

  if FTodoFields.CreationDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CreationDateField);
    if Assigned(Field) then
      ATodoItem.CreationDate := Field.AsDateTime;
  end;

  if FTodoFields.DueDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.DueDateField);
    if Assigned(Field) then
      ATodoItem.DueDate := Field.AsDateTime;
  end;

  if FTodoFields.CompletionDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompletionDateField);
    if Assigned(Field) then
      ATodoItem.CompletionDate := Field.AsDateTime;
  end;

  if FTodoFields.CompleteField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompleteField);
    if Assigned(Field) then
      ATodoItem.Complete := Field.AsBoolean;
  end;

  if FTodoFields.CompletionField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompletionField);
    if Assigned(Field) then
      ATodoItem.Completion := Field.AsInteger;
  end;

  if FTodoFields.ResourceField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ResourceField);
    if Assigned(Field) then
      ATodoItem.Resource := Field.AsString;
  end;

  if FTodoFields.ProjectField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ProjectField);
    if Assigned(Field) then
      ATodoItem.Project := Field.AsString;
  end;
  if FTodoFields.ImageField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ImageField);
    if Assigned(Field) then
      ATodoItem.ImageIndex := Field.AsInteger;
  end;
  if FTodoFields.TotalTimeField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.TotalTimeField);
    if Assigned(Field) then
       ATodoItem.TotalTime := Field.AsFloat;
  end;

end;

procedure TDBTodoList.DBWrite(D: TDataSet; ATodoItem: TTodoItem);
var
  Field: TField;

begin
  if (FTodoFields.KeyField <> '') and not FTodoFields.AutoIncKey then
  begin
    Field := D.FieldByName(FTodoFields.KeyField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.DBKey;
  end;

  if FTodoFields.SubjectField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.SubjectField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.Subject;
  end;

  if FTodoFields.CategoryField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CategoryField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.Category;
  end;

  if FTodoFields.NotesField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.NotesField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.Notes.Text;
  end;

  if FTodoFields.StatusField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.StatusField);
    if Assigned(Field) then
      Field.AsInteger := Integer(ATodoItem.Status);
  end;

  if FTodoFields.PriorityField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.PriorityField);
    if Assigned(Field) then
      Field.AsInteger := Integer(ATodoItem.Priority);
  end;

  if FTodoFields.CreationDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CreationDateField);
    if Assigned(Field) then
      Field.AsDateTime := ATodoItem.CreationDate;
  end;

  if FTodoFields.DueDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.DueDateField);
    if Assigned(Field) then
      Field.AsDateTime := ATodoItem.DueDate;
  end;

  if FTodoFields.CompletionDateField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompletionDateField);
    if Assigned(Field) then
      Field.AsDateTime := ATodoItem.CompletionDate;
  end;

  if FTodoFields.CompleteField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompleteField);
    if Assigned(Field) then
      Field.AsBoolean := ATodoItem.Complete;
  end;

  if FTodoFields.CompletionField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.CompletionField);
    if Assigned(Field) then
      Field.AsInteger := ATodoItem.Completion;
  end;

  if FTodoFields.ResourceField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ResourceField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.Resource;
  end;

  if FTodoFields.ProjectField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ProjectField);
    if Assigned(Field) then
      Field.AsString := ATodoItem.Project;
  end;

  if FTodoFields.ImageField <> '' then
  begin
    Field := D.FieldByName(FTodoFields.ImageField);
    if Assigned(Field) then
      Field.AsInteger := ATodoItem.ImageIndex;
  end;

  if FTodoFields.TotalTimeField <> '' then
  begin
     Field := D.FieldByName(FTodoFields.TotalTimeField);
     if Assigned(Field) then
        Field.AsFloat := ATodoItem.TotalTime;
  end;

end;

procedure TDBTodoList.DBInsert(ATodoItem: TTodoItem);
var
  GUID: TGUID;
  D: TDataSet;
  NewKey: string;

begin
  if CheckDataSet then
  begin
    if (ATodoItem.DBKey = '') and not FTodoFields.AutoIncKey then
    begin
      if Assigned(FOnCreateDBKey) then
      begin
        NewKey := '';
        FOnCreateDBKey(Self,ATodoItem,NewKey);
        ATodoItem.DBKey := NewKey;
      end
      else
      begin
        if not FTodoFields.AutoIncKey then
        begin
          CoCreateGUID(GUID);
          ATodoItem.DBKey := GUIDToString(GUID);
        end;
      end;
    end;
    D := FDataLink.DataSource.DataSet;
    FDBUpdating := True;
    D.Append;

    if FTodoFields.SubjectField <> '' then
      D.FieldbyName(FTodoFields.SubjectField).AsString := '';

    if FTodoFields.CompleteField <> '' then
      D.FieldbyName(FTodoFields.CompleteField).AsBoolean := False;

    if FTodoFields.CompletionField <> '' then
      D.FieldbyName(FTodoFields.CompletionField).AsInteger := 0;

    if FTodoFields.AutoIncKey then
    begin
      D.Post;
      ATodoItem.DBKey := D.FieldbyName(FTodoFields.KeyField).AsString;
      D.Edit;
    end
    else
    begin
      DBWrite(D,ATodoItem);
    end;

    FDBUpdating := False;
  end;
end;

procedure TDBTodoList.FreeItem(ATodoItem: TTodoItem);
begin
  DBDelete(ATodoItem);
  ATodoItem.Free;
end;

procedure TDBTodoList.DBUpdate(ATodoItem: TTodoItem);
var
  Field: TField;
  D: TDataSet;
begin
  if not CheckDataSet then
    Exit;

  if (ATodoItem.DBKey <> '') and (FTodoFields.KeyField <> '') then
  begin
    D := FDataLink.DataSource.DataSet;
    Field := D.FieldByName(FTodoFields.KeyField);
    if Assigned(Field) then
    begin
      FDBUpdating := True;

      FDataLink.DataSource.DataSet.Locate(FTodoFields.KeyField, ATodoItem.DBKey,[]);

      D.Edit;

      if (FTodoFields.KeyField <> '') and not FTodoFields.AutoIncKey  then
      begin
        Field := D.FieldByName(FTodoFields.KeyField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.DBKey;
      end;

      if FTodoFields.SubjectField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.SubjectField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.Subject;
      end;

      if FTodoFields.CategoryField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.CategoryField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.Category;
      end;

      if FTodoFields.NotesField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.NotesField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.Notes.Text;
      end;

      if FTodoFields.StatusField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.StatusField);
        if Assigned(Field) then
          Field.AsInteger := Integer(ATodoItem.Status);
      end;

      if FTodoFields.PriorityField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.PriorityField);
        if Assigned(Field) then
          Field.AsInteger := Integer(ATodoItem.Priority);
      end;

      if FTodoFields.CreationDateField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.CreationDateField);
        if Assigned(Field) then
          Field.AsDateTime := ATodoItem.CreationDate;
      end;

      if FTodoFields.DueDateField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.DueDateField);
        if Assigned(Field) then
          Field.AsDateTime := ATodoItem.DueDate;
      end;

      if FTodoFields.CompletionDateField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.CompletionDateField);
        if Assigned(Field) then
          Field.AsDateTime := ATodoItem.CompletionDate;
      end;

      if FTodoFields.CompleteField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.CompleteField);
        if Assigned(Field) then
          Field.AsBoolean := ATodoItem.Complete;
      end;

      if FTodoFields.CompletionField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.CompletionField);
        if Assigned(Field) then
          Field.AsInteger := ATodoItem.Completion;
      end;

      if FTodoFields.ResourceField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.ResourceField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.Resource;
      end;

      if FTodoFields.ProjectField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.ProjectField);
        if Assigned(Field) then
          Field.AsString := ATodoItem.Project;
      end;

      if FTodoFields.ImageField <> '' then
      begin
        Field := D.FieldByName(FTodoFields.ImageField);
        if Assigned(Field) then
          Field.AsInteger := ATodoItem.ImageIndex;
      end;

      if FTodoFields.TotalTimeField <> '' then
      begin
         Field := D.FieldByName(FTodoFields.TotalTimeField);
         if Assigned(Field) then
            Field.AsFloat := ATodoItem.TotalTime;
      end;

      D.Post;

      FDBUpdating := False;
    end;
  end;
end;

procedure TDBTodoList.UpdateItem(ATodoItem: TTodoItem);
begin
  DBUpdate(ATodoItem);
end;


procedure TDBTodoList.ImportFromCSV(FileName: string);
begin
  InputFromCSV(FileName, false);
end;


procedure TDBTodoList.SyncCursor;
var
  D: TDataSet;
  Field: TField;
  Key: string;
  i: Integer;

begin
  if CheckDataSet then
  begin
    if (FTodoFields.KeyField <> '') then
    begin
      D := FDataLink.DataSource.DataSet;
      Field := D.FieldByName(FTodoFields.KeyField);
      if Assigned(Field) then
      begin
        Key := Field.AsString;
        for i := 1 to Items.Count do
        begin
          if Items[i - 1].DBKey = Key then
          begin
            TodoListBox.ItemIndex := i - 1;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDBTodoList.SyncRecord;
var
  ATodoItem: TTodoItem;
  D: TDataSet;
begin
  if CheckDataSet then
  begin
    D := FDataLink.DataSource.DataSet;
    SyncCursor;
    if TodoListBox.ItemIndex >= 0 then
    begin
      ATodoItem := Items.Items[TodoListBox.ItemIndex];
      DBRead(D, ATodoItem);
    end;
  end;
end;

procedure TDBTodoList.ItemSelect(ATodoItem: TTodoItem);
var
  D: TDataSet;
begin
  inherited;
  if not Assigned(ATodoItem) then
    Exit;

  if CheckDataSet then
  begin
    if (FTodoFields.KeyField <> '') and (ATodoItem.DBKey <> '') then
    begin
      FDBUpdating := True;
      D := FDataLink.DataSource.DataSet;
      D.Locate(FTodoFields.KeyField,ATodoItem.DBKey,[]);
      FDBUpdating := False;
    end;
  end;
end;

procedure TDBTodoList.Reload;
begin
  SyncItems;
end;

{ TTodoFields }

constructor TTodoFields.Create(AOwner: TDBTodoList);
begin
  inherited Create;
  FDBTodoList := AOwner;
end;


{ TDBTodoListDataLink }

procedure TDBTodoListDataLink.ActiveChanged;
begin
  inherited;
  FDBTodoList.SyncItems;
end;

constructor TDBTodoListDataLink.Create(ADBTodoList: TDBTodoList);
begin
  inherited Create;
  FDBTodoList := ADBTodoList;
end;

procedure TDBTodoListDataLink.DataSetChanged;
begin
  inherited;
end;

procedure TDBTodoListDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
  if not FDBTodoList.FDBUpdating then
    FDBTodoList.SyncCursor;
end;

destructor TDBTodoListDataLink.Destroy;
begin
  inherited;
end;

procedure TDBTodoListDataLink.RecordChanged(Field: TField);
begin
  inherited;
  if not FDBTodoList.FDBUpdating then
    FDBTodoList.SyncRecord;
end;

end.
