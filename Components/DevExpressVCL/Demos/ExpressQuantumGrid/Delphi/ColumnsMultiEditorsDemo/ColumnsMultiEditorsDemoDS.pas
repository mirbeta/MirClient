unit ColumnsMultiEditorsDemoDS;

{$I cxVer.inc}

interface

uses
  Variants, cxCustomData, cxGridTableView;

type
  TSkillDataSource = class(TcxCustomDataSource)
  private
    FTableView: TcxGridTableView;
    FCommunicationLevelCount: Integer;
    FLanguagesCount: Integer;
    Grades: Variant;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
        AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(ATableView: TcxGridTableView;
        ACommunicationLevelCount, ALanguagesCount: Integer);
  end;

const
  SkillCount = 6;

implementation

uses SysUtils;

{ TSkillDataSource }

const
  NameCount = 5;
  Names: Array[0..NameCount - 1] of string = ('Jerry Campbell', 'Ryan Fischer',
                'Tom Hamlett', 'Steve Lee', 'Jeffrey McClain');
  Skills: Array[0..SkillCount - 1] of string = ('Programming experiments (in years)',
                'Primary Language', 'Secondary Language', 'Communication',
                'Start working from', 'Custom Information');

constructor TSkillDataSource.Create(ATableView: TcxGridTableView;
        ACommunicationLevelCount, ALanguagesCount: Integer);
var
  I: Integer;
begin
  inherited Create;
  FTableView := ATableView;
  FCommunicationLevelCount := ACommunicationLevelCount;
  FLanguagesCount := ALanguagesCount;
  Grades := VarArrayCreate([0, NameCount * SkillCount - 1], varVariant);
  Randomize;
  for I := 0 to NameCount - 1 do
  begin
    Grades[I * SkillCount] := 1 + Random(10);
    Grades[I * SkillCount + 1] := Random(FLanguagesCount - 1);
    Grades[I * SkillCount + 2] := Random(FLanguagesCount - 1);
    while Grades[I * SkillCount + 1]  = Grades[I * SkillCount + 2] do
      Grades[I * SkillCount + 2] := Random(FLanguagesCount - 1);
    Grades[I * SkillCount + 3] := Random(FCommunicationLevelCount - 1);
    Grades[I * SkillCount + 4] := EncodeDate(1992 + Random(10), 1 + Random(12), 1 + Random(27));
    Grades[I * SkillCount + 5] := 'Put additional information here';
  end;
end;

function TSkillDataSource.GetRecordCount: Integer;
begin
  Result := NameCount * SkillCount;
end;

function TSkillDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AColumnId: Integer;
begin
  AColumnId := FTableView.Columns[Integer(AItemHandle)].DataBinding.Item.ID;
  case AColumnId of
    0: Result := Names[Integer(ARecordHandle) div SkillCount];
    1: Result := Skills[Integer(ARecordHandle) - (Integer(ARecordHandle) div SkillCount) * SkillCount];
    2: Result := Grades[Integer(ARecordHandle)];
    else Result := Null;
  end;
end;

procedure TSkillDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
  if FTableView.Columns[Integer(AItemHandle)].DataBinding.Item.ID = 2 then
    Grades[Integer(ARecordHandle)] := AValue;
end;

end.
