{***************************************************************************}
{ TAdvStringGrid component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996-2013                                          }
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

unit AsgImport;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BaseGrid, AdvGrid, AdvObj, Grids, ExtCtrls, AdvColumnSetter
  ,Types ,Variants
  ;

type
  TDelimiters = (sepDotComma,sepMinus,sepComma,sepUnderScore,sepAutomatic,sepCustom);

  TAdvGridImportText = class(TPersistent)
  private
    FDelimited: string;
    FCaption: string;
    FFixed: string;
    FAutomatic: string;
    FCustom: string;
    FFirstRowHeaders: string;
    FButtonOK: string;
    FButtonCancel: string;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Fixed: string read FFixed write FFixed;
    property Automatic: string read FAutomatic write FAutomatic;
    property Custom: string read FCustom write FCustom;
    property FirstRowHeaders: string read FFirstRowHeaders write FFirstRowHeaders;
    property Caption: string read FCaption write FCaption;
    property Delimited: string read FDelimited write FDelimited;
    property ButtonOK: string read FButtonOK write FButtonOk;
    property ButtonCancel: string read FButtonCancel write FButtonCancel;
  end;

  TAsgImportForm = class(TForm)
    gpbDelimited: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    rdbDelimited: TRadioButton;
    rdbFixed: TRadioButton;
    rdbDel1: TRadioButton;
    rdbDel2: TRadioButton;
    rdbDel3: TRadioButton;
    rdbDel4: TRadioButton;
    rdbDel5: TRadioButton;
    txtCustom: TEdit;
    ScrollBox1: TScrollBox;
    GridPreview: TAdvStringGrid;
    AdvColumnSetter1: TAdvColumnSetter;
    ScrollTimer: TTimer;
    chkColumnHeaders: TCheckBox;
    rdbAutomatic: TRadioButton;
    constructor Create(AOwner: TComponent); override;
    procedure UpdateGrid;
    procedure btnCancelClick(Sender: TObject);
    procedure ImportSettingsChanged(Sender: TObject);
    procedure DelimiterSettingsChanged(Sender: TObject);
    procedure txtCustomKeyPress(Sender: TObject; var Key: Char);
    procedure AdvColumnSetter1ColumnSetterChanged(Sender: TObject; Index,
      Position: Integer);
    procedure AdvColumnSetter1ColumnSetterInsert(Sender: TObject;
      Position: Integer);
    procedure AdvColumnSetter1ColumnSetterDeleted(Sender: TObject;
      Index: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure ScrollTimerTimer(Sender: TObject);
    procedure AdvColumnSetter1ColumnSetterChanging(Sender: TObject; Index,
      Position: Integer);
    procedure FormShow(Sender: TObject);
  private
    FGrid: TAdvStringGrid;
    FFileName: String;
    FScrollDelta: integer;
    FNewPos: integer;
    FOldPos: integer;
    FScrollWidth: Integer;
    FAutoSizeOnLoad: boolean;
    FIsFixed: boolean;
    FDisplayRowCount: integer;
    FLoadRowCount: integer;
    FFirstRowHeaders: boolean;
  public
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property FileName: String read FFileName write FFileName;
    property AutoSizeOnLoad: boolean read FAutoSizeOnLoad write FAutoSizeOnLoad;
    property ColumnSetter: TAdvColumnSetter read AdvColumnSetter1;
    property IsFixed: boolean read FIsFixed write FIsFixed;
    property DisplayRowCount: integer read FDisplayRowCount write FDisplayRowCount;
    property LoadRowCount: integer read FLoadRowCount write FLoadRowCount;
    property FirstRowHeaders: boolean read FFirstRowHeaders write FFirstRowHeaders;
  end;

  TImportType = (itFixed, itDelimited);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridImportDialog = class(TCommonDialog)
  private
    FGrid: TAdvStringGrid;
    FForm: TAsgImportForm;
    FFilter: string;
    FFilterIndex: integer;
    FInitialDir: string;
    FFileName: string;
    FTitle: string;
    FDelimiter: TDelimiters;
    FCustomDelimiter: string;
    FTextSettings: TAdvGridImportText;
    FAutoSizeOnLoad: boolean;
    FOnFormCreated: TNotifyEvent;
    FColumns: TIntList;
    FImportType: TImportType;
    FRowsToLoad: integer;
    FRowsToDisplay: integer;
    FFirstRowHeaders: boolean;
    procedure SetTextSettings(const Value: TAdvGridImportText);
  protected
    function FileExecute(sFileName: String): Boolean; overload;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function OpenAndExecute: boolean;
    property Form: TAsgImportForm read FForm;
    property FileName: String read FFileName write FFileName;
    property Columns: TIntList read FColumns;
    property FirstRowHeaders: boolean read FFirstRowHeaders write FFirstRowHeaders;
  published
    property AutoSizeOnLoad: boolean read FAutoSizeOnLoad write FAutoSizeOnLoad default true;
    property CustomDelimiter: string read FCustomDelimiter write FCustomDelimiter;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property TextSettings: TAdvGridImportText read FTextSettings write SetTextSettings;
    property OnFormCreated: TNotifyEvent read FOnFormCreated write FOnFormCreated;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: integer read FFilterIndex write FFilterIndex default 0;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Delimiter: TDelimiters read FDelimiter write FDelimiter default sepDotComma;
    property ImportType: TImportType read FImportType write FImportType default itDelimited;
    property Title: string read FTitle write FTitle;
    property RowsToDisplay: integer read FRowsToDisplay write FRowsToDisplay default 10;
    property RowsToLoad: integer read FRowsToLoad write FRowsToLoad default -1;
  end;

  type
  TMouseState = (msNone, msDown, msMoving);

implementation


{$R *.DFM}

constructor TAsgImportForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AdvColumnSetter1.Enabled := false;
  AdvColumnSetter1.Left := 0;
  AdvColumnSetter1.Width := GridPreview.Width;
  GridPreview.Left := 0;
  GridPreview.FixedCols := 0;
  GridPreview.FixedRows := 0;
  GridPreview.Font.Name := 'Courier New';
  GridPreview.Font.Size := 9;
  GridPreview.XYOffset := point(0,0);
  FScrollWidth := ScrollBox1.Width;
end;

constructor TAdvGridImportDialog.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizeOnLoad := true;
  FTextSettings := TAdvGridImportText.Create(Self);
  FFilter := 'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|Ascii files (*.asc)|*.asc|All files (*.*)|*.*';
  FColumns := TIntList.Create(-1,-1);
  FImportType := itDelimited;
  FRowsToDisplay := 10;
  FRowsToLoad := -1;
end;

destructor TAdvGridImportDialog.Destroy;
begin
  FTextSettings.Free;
  FColumns.Free;
  inherited;
end;

function TAdvGridImportDialog.Execute: Boolean;
begin
  Result := FileExecute(Self.FileName);
end;

function TAdvGridImportDialog.FileExecute(sFileName: String): Boolean;
var
  Delimiter: string;
begin
  if not Assigned(Grid) then
  begin
    raise Exception.Create('The dialog does not have a grid component assigned.');
    Result := False;
    Exit;
  end;

  if FileName = '' then
  begin
    raise Exception.Create('No filename specified');
    Result := false;
    Exit;
  end;

  if csDesigning in ComponentState then
    FForm := TAsgImportForm.Create(Application)
  else
    FForm := TAsgImportForm.Create(Self);

  if Assigned(FOnFormCreated) then
    FOnFormCreated(Self);

  FForm.FirstRowHeaders := FirstRowHeaders;

  with FForm do
    try

      AutoSizeOnLoad := Self.AutoSizeOnLoad;
      Grid := Self.Grid;
      FileName := sFileName;
      ColumnSetter.Columns.Assign(Columns);
      DisplayRowCount := RowsToDisplay;
      LoadRowCount := RowsToLoad;

      case self.Delimiter of
      sepDotComma: Delimiter := ';';
      sepMinus: Delimiter := '-';
      sepComma: Delimiter := ',';
      sepUnderscore: Delimiter := '_';
      sepAutomatic : Delimiter := #0;
      sepCustom: Delimiter := CustomDelimiter;
      end;

      case self.Delimiter of
      sepDotComma: rdbDel1.Checked := true;
      sepMinus: rdbDel2.Checked := true;
      sepComma: rdbDel3.Checked := true;
      sepUnderscore: rdbDel4.Checked := true;
      sepAutomatic : rdbAutomatic.Checked := true;
      sepCustom: rdbDel5.Checked := true;
      end;

      txtCustom.Text := CustomDelimiter;

      if FileName <> '' then
      begin
        if Length(Delimiter) > 0 then
          GridPreview.Delimiter := Delimiter[1]
        else
          GridPreview.Delimiter := #0;
        GridPreview.ColCount := GridPreview.FixedCols + 1;
        GridPreview.LoadFromCSV(FileName, FRowsToDisplay);
        GridPreview.AutoSizeColumns(true,4);
      end;

      Caption := TextSettings.Caption;
      rdbDelimited.Caption := TextSettings.Delimited;
      rdbAutomatic.Caption := TextSettings.Automatic;
      rdbDel5.Caption := TextSettings.Custom;
      chkColumnHeaders.Caption := TextSettings.FirstRowHeaders;
      chkColumnHeaders.Checked := FirstRowHeaders;
      rdbFixed.Caption := TextSettings.Fixed;
      btnOK.Caption := TextSettings.ButtonOK;
      btnOK.ModalResult := mrOK;
      btnCancel.Caption := TextSettings.ButtonCancel;
      btnCancel.ModalResult := mrCancel;
      btnOK.Default := true;
      btnCancel.Cancel := true;

      IsFixed := (ImportType = itFixed);

      Result := (ShowModal = mrOK);

      self.FirstRowHeaders := FForm.FirstRowHeaders;

      if rdbDel1.Checked then
        self.Delimiter := sepDotComma;
      if rdbDel2.Checked then
        self.Delimiter := sepMinus;
      if rdbDel3.Checked then
        self.Delimiter := sepComma;
      if rdbDel4.Checked then
        self.Delimiter := sepUnderScore;
      if rdbAutomatic.Checked then
        self.Delimiter := sepAutomatic;
      if rdbDel5.Checked then
        self.Delimiter := sepCustom;

    finally
      Free;
    end;
end;

procedure TAdvGridImportDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Grid) then
  begin
    FGrid := nil;
  end;
end;

function TAdvGridImportDialog.OpenAndExecute: boolean;
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(Self);

  // od properties
  od.Filter := FFilter;
  od.FilterIndex := FFilterIndex;
  od.InitialDir := FInitialDir;
  od.Title := FTitle;
  od.Options := [ofFileMustExist, ofNoChangeDir];

  Result := false;

  try
    if od.Execute then
    begin
      self.FileName := od.FileName;
      Result := Self.FileExecute(od.FileName);
    end;
  finally
    od.Free;
  end;

end;

procedure TAdvGridImportDialog.SetTextSettings(const Value: TAdvGridImportText);
begin
  FTextSettings.Assign(Value);
end;

procedure TAsgImportForm.AdvColumnSetter1ColumnSetterChanged(Sender: TObject;
  Index, Position: Integer);
begin
  UpdateGrid;
end;

procedure TAsgImportForm.AdvColumnSetter1ColumnSetterChanging(Sender: TObject;
  Index, Position: Integer);
begin
  FNewPos := Position * AdvColumnSetter1.ColWidth;
  if FNewPos > FOldPos then
  begin
    if ((Position * AdvColumnSetter1.ColWidth) > (FScrollWidth - (FScrollWidth div 10))) then
    begin
      FScrollDelta := 15;
    end;
  end;

  if (FNewPos < FOldPos) then
  begin
    if ((Position * AdvColumnSetter1.ColWidth) < (FScrollWidth - (FScrollWidth - (FScrollWidth div 10))) + ScrollBox1.HorzScrollBar.Position) then
    begin
      FScrollDelta := -15;
    end;
  end;

  if ((Position * AdvColumnSetter1.ColWidth) > (FScrollWidth - (FScrollWidth- (FScrollWidth div 10))) + ScrollBox1.HorzScrollBar.Position) and ((Position * AdvColumnSetter1.ColWidth) < (FScrollWidth - (FScrollWidth div 10))) then
    FScrollDelta := 0;

  FOldPos := FNewPos;
end;

procedure TAsgImportForm.AdvColumnSetter1ColumnSetterDeleted(Sender: TObject;
  Index: Integer);
begin
  UpdateGrid;
end;

procedure TAsgImportForm.AdvColumnSetter1ColumnSetterInsert(Sender: TObject;
  Position: Integer);
begin
  UpdateGrid;
end;

procedure TAsgImportForm.btnCancelClick(Sender: TObject);
begin
  FirstRowHeaders := chkColumnHeaders.Checked;
  ModalResult := mrCancel;
end;

procedure TAsgImportForm.btnOKClick(Sender: TObject);
begin
  //LOAD VALUES IN GRID
  Grid.Clear;
  Grid.SaveFixedCells := chkColumnHeaders.Checked;
  if rdbDelimited.Checked then
  begin
    Grid.Delimiter := GridPreview.Delimiter;
    Grid.ColCount := Grid.FixedCols + 1;    
    Grid.LoadFromCSV(FileName, LoadRowCount);
  end
  else
  begin
    //ADD MAXIMUM
    AdvColumnSetter1.Columns.Add(0);    
    AdvColumnSetter1.Columns.Add(AdvColumnSetter1.MaxLength);
    Grid.LoadFromFixed(FileName, AdvColumnSetter1.Columns);
  end;

  if AutoSizeOnLoad then
    Grid.AutoSizeColumns(true, 4);
  FirstRowHeaders := chkColumnHeaders.Checked;
  ModalResult := mrOK;
  //Close;
end;

procedure TAsgImportForm.DelimiterSettingsChanged(Sender: TObject);
var
  i: integer;
begin
  GridPreview.Clear;
  i := TRadioButton(Sender).Tag;
  case i of
  1: // (;)
      GridPreview.Delimiter := ';';
  2: // (-)
      GridPreview.Delimiter := '-';
  3: // (,)
      GridPreview.Delimiter := ',';
  4: // (_)
      GridPreview.Delimiter := '_';
  5: // Automatic
      GridPreview.Delimiter := #0;
  6: // Custom
    begin
      if (txtCustom.Text <> '') then
        GridPreview.Delimiter := txtCustom.Text[1]
      else
        GridPreview.Delimiter := #0;
    end;
  end;

  GridPreview.ColCount := GridPreview.FixedCols + 1;
  GridPreview.LoadFromCSV(FileName, FDisplayRowCount);

  txtCustom.Enabled := (i = 6);

  GridPreview.AutoSizeColumns(true,4);
end;

procedure TAsgImportForm.UpdateGrid;
var
  I: Integer;
  il: TIntList;
begin
  if AdvColumnSetter1.Columns.Count <= 1 then
  begin
    il := TIntList.Create(-1,-1);
    il.Add(0);
    il.Add($FFFF);
    GridPreview.LoadFromFixed(FFileName, il, false, FDisplayRowCount);
    il.Free;
    GridPreview.AutoSizeColumns(false,20);
  end
  else
  begin
    il := TIntList.Create(-1,-1);
    il.Add(0);
    for i := 0 to AdvColumnSetter1.Columns.Count - 1 do
    begin
      il.Add(AdvColumnSetter1.Columns[i] + 1);
    end;
    il.Add(AdvColumnSetter1.MaxLength);
    GridPreview.LoadFromFixed(FFileName, il, false, FDisplayRowCount);

    il.Free;
    GridPreview.AutoSizeColumns(false,-6);
    if GridPreview.ColCount > 0 then
    GridPreview.ColWidths[GridPreview.ColCount - 1] := GridPreview.ColWidths[GridPreview.ColCount - 1] + 20;
  end;
end;

procedure TAsgImportForm.FormShow(Sender: TObject);
begin
  rdbFixed.Checked := FIsFixed;
end;


procedure TAsgImportForm.ImportSettingsChanged(Sender: TObject);
var
  f: TextFile;
  str: string;
  max: Integer;
  current: Integer;
  i: integer;
begin
  GridPreview.Clear;
  i := TRadioButton(Sender).Tag;
  case i of
  1:
    begin
      gpbDelimited.Enabled := true;
      AdvColumnSetter1.Enabled := false;
      DelimiterSettingsChanged(Sender);
      AdvColumnSetter1.Invalidate;
    end;
  2:
    begin
      gpbDelimited.Enabled := false;
      AdvColumnSetter1.Enabled := true;
      AdvColumnSetter1.Invalidate;

      AssignFile(f, FileName);
      Reset(f);
      Max := 0;
      while not Eof(f) do
      begin
        ReadLn(f,str);
        current := Length(str) + 1;
        if (current > max) then
        begin
          //AdvColumnSetter1.MaxLength := ((current div 10) + 1) * 10;
          max := current;
        end;
      end;

      CloseFile(f);

      AdvColumnSetter1.MaxLength := max;

      //GridPreview.Width := (AdvColumnSetter1.MaxLength + 1) * AdvColumnSetter1.ColWidth;
      AdvColumnSetter1.Width := GridPreview.Width;
      UpdateGrid;
    end;
  end;

end;

procedure TAsgImportForm.ScrollTimerTimer(Sender: TObject);
begin
  if FScrollDelta <> 0 then
  begin
    ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + FScrollDelta;
    FScrollWidth := FScrollWidth + FScrollDelta;
    FScrollDelta := 0;
  end;
end;

procedure TAsgImportForm.txtCustomKeyPress(Sender: TObject; var Key: Char);
begin
  GridPreview.Clear;
  if (Key <> #13) and (Key <> #8) and (Key <> #0) then
  begin
    txtCustom.Text := Key;
    GridPreview.Delimiter := Key;
    GridPreview.LoadFromCSV(FileName, DisplayRowCount);
    GridPreview.AutoSizeColumns(false, 0);
  end;
end;

{ TAdvGridImportText }

procedure TAdvGridImportText.Assign(Source: TPersistent);
begin
  if (Source is TAdvGridImportText) then
  begin
    FCaption := (Source as TAdvGridImportText).Caption;
    FDelimited := (Source as TAdvGridImportText).Delimited;
  end;
end;

constructor TAdvGridImportText.Create(AOwner: TComponent);
begin
  inherited Create;
  FCaption := 'Import settings';
  FDelimited := 'Delimited';
  FFixed := 'Fixed (click the column header to set split positions)';
  FCustom := 'Custom';
  FButtonOK := 'OK';
  FButtonCancel := 'Cancel';
  FAutomatic := 'Automatic';
  FFirstRowHeaders := 'First row headers';
end;




end.


