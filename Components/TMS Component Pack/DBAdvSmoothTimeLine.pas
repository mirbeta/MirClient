{***************************************************************************}
{ TDBAdvSmoothTimeLine component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
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

unit DBAdvSmoothTimeLine;

{$I TMSDEFS.INC}

interface

uses
  AdvSmoothTimeLine, DB, Graphics, Classes, SysUtils, Windows, Math, Dialogs;

type
  TDBAdvSmoothTimeLine = class;
  TDBAdvSmoothTimeLineBarIndicator = class(TAdvSmoothTimeLineBarIndicator);
  TDBAdvSmoothTimeLineBarSection = class(TAdvSmoothTimeLineBarSection);

  TDBAdvSmoothTimeLineDataBinding = class(TPersistent)
  private
    FOwner: TDBAdvSmoothTimeLine;
    FOnChange: TNotifyEvent;
    FIndicatorPicture: string;
    FSectionFixedSize: string;
    FSectionEndTime: string;
    FSectionCaption: string;
    FIndicatorShape: string;
    FSectionFixedPosition: string;
    FIndicatorTime: string;
    FSectionHint: string;
    FIndicatorFixed: string;
    FSectionStartTime: string;
    FIndicatorAnnotation: String;
    FKey: String;
    FSectionColor: String;
    FIndicatorColor: String;
    FIndicatorAnnotationColor: String;
    FIndicatorAnnotationPosition: String;
    FSectionColorTo: String;
    FIndicatorColorTo: String;
    FIndicatorAnnotationColorTo: String;
    FSectionColorMirror: String;
    FSectionColorMirrorTo: String;
    FIndicatorAnnotationTextColor: String;
    FIndicatorAnnotationImageIndex: String;
    function GetDBTimeLine: TDBAdvSmoothTimeLine;
    procedure SetIndicatorAnnotation(const Value: String);
    procedure SetIndicatorFixed(const Value: string);
    procedure SetIndicatorPicture(const Value: string);
    procedure SetIndicatorShape(const Value: string);
    procedure SetIndicatorTime(const Value: string);
    procedure SetSectionCaption(const Value: string);
    procedure SetSectionEndTime(const Value: string);
    procedure SetSectionFixedPosition(const Value: string);
    procedure SetSectionFixedSize(const Value: string);
    procedure SetSectionHint(const Value: string);
    procedure SetSectionStartTime(const Value: string);
    procedure SetKey(const Value: String);
    procedure SetIndicatorColor(const Value: String);
    procedure SetSectionColor(const Value: String);
    procedure SetIndicatorAnnotationColor(const Value: String);
    procedure SetIndicatorAnnotationPosition(const Value: String);
    procedure SetIndicatorColorTo(const Value: String);
    procedure SetSectionColorMirror(const Value: String);
    procedure SetSectionColorMirrorTo(const Value: String);
    procedure SetSectionColorTo(const Value: String);
    procedure SetIndicatorAnnotationTextColor(const Value: String);
    procedure SetIndicatorAnnotationImageIndex(const Value: String);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TDBAdvSmoothTimeLine);
    destructor Destroy; override;
    property DBAdvSmoothTimeLine: TDBAdvSmoothTimeLine read GetDBTimeLine;
  published
    property Key: String read FKey write SetKey;
    property IndicatorTime: string read FIndicatorTime write SetIndicatorTime;
    property IndicatorColor: String read FIndicatorColor write SetIndicatorColor;
    property IndicatorColorTo: String read FIndicatorColorTo write SetIndicatorColorTo;
    property IndicatorAnnotationColor: String read FIndicatorAnnotationColor write SetIndicatorAnnotationColor;
    property IndicatorAnnotationPosition: String read FIndicatorAnnotationPosition write SetIndicatorAnnotationPosition;
    property IndicatorAnnotation: String read FIndicatorAnnotation write SetIndicatorAnnotation;
    property IndicatorAnnotationTextColor: String read FIndicatorAnnotationTextColor write SetIndicatorAnnotationTextColor;
    property IndicatorAnnotationImageIndex: String read FIndicatorAnnotationImageIndex write SetIndicatorAnnotationImageIndex;
    property IndicatorFixed: string read FIndicatorFixed write SetIndicatorFixed;
    property IndicatorShape: string read FIndicatorShape write SetIndicatorShape;
    property IndicatorPicture: string read FIndicatorPicture write SetIndicatorPicture;
    property SectionCaption: string read FSectionCaption write SetSectionCaption;
    property SectionStartTime: string read FSectionStartTime write SetSectionStartTime;
    property SectionEndTime: string read FSectionEndTime write SetSectionEndTime;
    property SectionFixedPosition: string read FSectionFixedPosition write SetSectionFixedPosition;
    property SectionFixedSize: string read FSectionFixedSize write SetSectionFixedSize;
    property SectionHint: string read FSectionHint write SetSectionHint;
    property SectionColor: String read FSectionColor write SetSectionColor;
    property SectionColorTo: String read FSectionColorTo write SetSectionColorTo;
    property SectionColorMirror: String read FSectionColorMirror write SetSectionColorMirror;
    property SectionColorMirrorTo: String read FSectionColorMirrorTo write SetSectionColorMirrorTo;
  end;

  TDBAdvSmoothTimeLineDataLink = class(TDataLink)
  private
    FTimeLine: TDBAdvSmoothTimeLine;
    FEditChange: boolean;
    FEditUpdate: boolean;
    FLoading: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(ATimeLine: TDBAdvSmoothTimeLine);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property ListBox: TDBAdvSmoothTimeLine read FTimeLine;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSmoothTimeLine = class(TAdvSmoothTimeLine)
  private
    FDataLink: TDBAdvSmoothTimeLineDataLink;
    FDataBinding: TDBAdvSmoothTimeLineDataBinding;
    function GetDataSource: TDataSource;
    procedure SetDataBinding(const Value: TDBAdvSmoothTimeLineDataBinding);
    procedure SetDataSource(const Value: TDataSource);
    procedure OnDataBindingChanged(Sender: TObject);
  protected
    procedure DoSectionPositionChanged(Sender: TObject; section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime); override;
    procedure DoIndicatorPositionChanged(Sender: TObject; indicator: TAdvSmoothTimeLineBarIndicator; Position: TDateTime); override;
    procedure DoIndicatorDown(Sender: TObject; indicator: TAdvSmoothTimeLineBarIndicator); override;
    procedure DoSectionDown(Sender: TObject; section: TAdvSmoothTimeLineBarSection); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData;
    function CheckDataSet: Boolean;
  published
    property DataBinding: TDBAdvSmoothTimeLineDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{ TDBAdvSmoothTimeLine }

function TDBAdvSmoothTimeLine.CheckDataSet: Boolean;
begin
  Result := false;
  if Assigned(DataSource) then
    Result := Assigned(DataSource.DataSet) and DataSource.DataSet.Active
end;

constructor TDBAdvSmoothTimeLine.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDBAdvSmoothTimeLineDataLink.Create(Self);
  FDataLink.FTimeLine := Self;

  FDataBinding := TDBAdvSmoothTimeLineDataBinding.Create(Self);
  FDataBinding.OnChange := OnDataBindingChanged;
end;

destructor TDBAdvSmoothTimeLine.Destroy;
begin
  FDataBinding.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvSmoothTimeLine.DoIndicatorDown(Sender: TObject;
  indicator: TAdvSmoothTimeLineBarIndicator);
begin
  inherited;
  if (DataBinding.Key <> '') and (indicator.DBKeyValue <> '') then
    DataSource.DataSet.Locate(DataBinding.Key, indicator.DBKeyValue, []);
end;

procedure TDBAdvSmoothTimeLine.DoIndicatorPositionChanged(Sender: TObject;
  indicator: TAdvSmoothTimeLineBarIndicator; Position: TDateTime);
begin
  inherited;
  if (DataBinding.Key <> '') and (indicator.DBKeyValue <> '') then
  begin
    DataSource.DataSet.Edit;
    DataSource.DataSet.FieldByName(DataBinding.IndicatorTime).AsDateTime := Position;
    DataSource.DataSet.Post;
  end;
end;

procedure TDBAdvSmoothTimeLine.DoSectionDown(Sender: TObject;
  section: TAdvSmoothTimeLineBarSection);
begin
  inherited;
  if (DataBinding.Key <> '') and (section.DBKeyValue <> '') then
    DataSource.DataSet.Locate(DataBinding.Key, section.DBKeyValue, []);
end;

procedure TDBAdvSmoothTimeLine.DoSectionPositionChanged(Sender: TObject;
  section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime);
begin
  inherited;
  if (DataBinding.Key <> '') and (section.DBKeyValue <> '') then
  begin
    DataSource.DataSet.Edit;
    DataSource.DataSet.FieldByName(DataBinding.SectionStartTime).AsDateTime := StartTime;
    DataSource.DataSet.FieldByName(DataBinding.SectionEndTime).AsDateTime := EndTime;
    DataSource.DataSet.Post;
  end;
end;

function TDBAdvSmoothTimeLine.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function Darker(Color: TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

procedure TDBAdvSmoothTimeLine.LoadData;
var
  cb: TBookmark;
  ind, secstart, secend: TDateTime;
  indan, seccap, sechint: String;
  indf, secfpos, secfsize: Boolean;
  indCol, indcolto, indancol,
  seccol, seccolto, seccolmi, seccolmito: Tcolor;
  indanntxtcol, inds, indsann, indsannimg: integer;
  aField: TField;
  blobf: TBlobField;
  s: TStream;
  timelineind: TAdvSmoothTimeLineBarIndicator;
  timelinesec: TAdvSmoothTimeLineBarSection;
begin
  if not CheckDataSet then
    Exit;

  BeginUpdate;

  cb := DataSource.DataSet.GetBookMark;
  DataSource.DataSet.First;
  DataSource.DataSet.DisableControls;

  TimeLineIndicators.Clear;
  TimeLineSections.Clear;

  while not DataSource.DataSet.Eof do
  begin
    if DataBinding.IndicatorTime <> '' then
    begin
      ind := DataSource.DataSet.FieldByName(DataBinding.IndicatorTime).AsDateTime;
      timelineind :=  TimeLineIndicators.Add;
      if DataBinding.Key <> '' then
        TDBAdvSmoothTimeLineBarIndicator(timelineind).FDBKeyvalue := DataSource.DataSet.FieldByName(DataBinding.Key).AsString;
      with timelineind do
      begin
        if DataBinding.IndicatorAnnotation <> '' then
          indan := DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotation).AsString;

        indf := false;
        if DataBinding.IndicatorFixed <> '' then
          indf := DataSource.DataSet.FieldByName(DataBinding.IndicatorFixed).AsBoolean;

        inds := 3;
        if DataBinding.IndicatorShape <> '' then
          inds := DataSource.DataSet.FieldByName(DataBinding.IndicatorShape).AsInteger;

        if (DataBinding.IndicatorPicture <> '') then
        begin
          aField := FDataLink.DataSet.Fieldbyname(DataBinding.IndicatorPicture);
          if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
          begin
            blobf := aField as TBlobField;
            s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
            try
              s.Position := 0;
              Picture.LoadFromStream(s);
            finally
              s.Free;
            end;
          end;
        end;

        indCol := clNone;
        if DataBinding.IndicatorColor <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.IndicatorColor).AsString <> '' then
            indCol := DataSource.DataSet.FieldByName(DataBinding.IndicatorColor).AsInteger;
        end;

        if indcol = clNone then
          indCol := DefaultIndicator.Color;

        indColto := clNone;
        if DataBinding.IndicatorColorTo <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.IndicatorColorTo).AsString <> '' then
            indColto := DataSource.DataSet.FieldByName(DataBinding.IndicatorColorTo).AsInteger;
        end;

        if indcolto = clNone then
          indColTo := DefaultIndicator.ColorTo;

        indanCol := clNone;
        if DataBinding.IndicatorAnnotationColor <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotationColor).AsString <> '' then
            indanCol := DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotationColor).AsInteger;
        end;

        if indancol = clNone then
          indanCol := DefaultIndicator.AnnotationColor;

        indsann := 0;
        if DataBinding.IndicatorAnnotationPosition <> '' then
          indsann := DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotationPosition).AsInteger;

        indsannimg := -1;
        if DataBinding.IndicatorAnnotationImageIndex <> '' then
          indsannimg := DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotationImageIndex).AsInteger;


        indanntxtcol := clNone;
        if DataBinding.IndicatorAnnotationTextColor <> '' then
          indanntxtcol := DataSource.DataSet.FieldByName(DataBinding.IndicatorAnnotationTextColor).AsInteger;

        if indanntxtcol = clNone then
          indanntxtcol := DefaultIndicator.AnnotationTextColor;

        AnnotationTextColor := indanntxtcol;
        AnnotationPosition := TAdvSmoothTimeLineBarAnnotationPosition(indsann);
        Color := indCol;
        ColorTo := indColTo;
        AnnotationColor := indancol;
        Position := ind;
        Annotation := indan;
        Fixed := indf;
        AnnotationImageIndex := indsannimg;
        Shape := TAdvSmoothTimeLineBarIndicatorShape(inds);
      end;
    end;

    if (DataBinding.SectionStartTime <> '') and (DataBinding.SectionEndTime <> '') then
    begin
      secstart := DataSource.DataSet.FieldByName(DataBinding.SectionStartTime).AsDateTime;
      secend := DataSource.DataSet.FieldByName(DataBinding.SectionEndTime).AsDateTime;
      timelinesec := TimeLineSections.Add;
      if DataBinding.Key <> '' then
        TDBAdvSmoothTimeLineBarSection(timelinesec).FDBKeyvalue := DataSource.DataSet.FieldByName(DataBinding.Key).AsString;

      with timelinesec do
      begin
        if DataBinding.SectionCaption <> '' then
          seccap := DataSource.DataSet.FieldByName(DataBinding.SectionCaption).AsString;

        secfpos := false;
        if DataBinding.SectionFixedPosition <> '' then
          secfpos := DataSource.DataSet.FieldByName(DataBinding.SectionFixedPosition).AsBoolean;

        secfsize := false;
        if DataBinding.SectionFixedSize <> '' then
          secfsize := DataSource.DataSet.FieldByName(DataBinding.SectionFixedSize).AsBoolean;

        if DataBinding.SectionHint <> '' then
          sechint := DataSource.DataSet.FieldByName(DataBinding.SectionHint).AsString;

        seccol := clNone;
        if DataBinding.SectionColor <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.SectionColor).AsString <> '' then
            seccol := DataSource.DataSet.FieldByName(DataBinding.SectionColor).AsInteger;
        end;

        if seccol = clNone then
          seccol := DefaultSectionFill.Color;

        seccolto := clNone;
        if DataBinding.SectionColorTo <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.SectionColorTo).AsString <> '' then
            seccolto := DataSource.DataSet.FieldByName(DataBinding.SectionColorTo).AsInteger;
        end;

        seccolmi := clNone;
        if DataBinding.SectionColorMirror <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.SectionColorMirror).AsString <> '' then
            seccolmi := DataSource.DataSet.FieldByName(DataBinding.SectionColorMirror).AsInteger;
        end;

        seccolmito := clNone;
        if DataBinding.SectionColorMirrorTo <> '' then
        begin
          if DataSource.DataSet.FieldByName(DataBinding.SectionColorMirrorTo).AsString <> '' then
            seccolmito := DataSource.DataSet.FieldByName(DataBinding.SectionColorMirrorTo).AsInteger;
        end;

        Fill.Color := seccol;
        Fill.ColorTo := seccolto;
        Fill.ColorMirror := seccolmi;
        Fill.ColorMirrorTo := seccolmito;
        StartTime := secstart;
        EndTime := secend;
        Caption := seccap;
        FixedPosition := secfpos;
        FixedSize := secfsize;
        Hint := sechint;
      end;
    end;
    DataSource.DataSet.Next;
  end;

  DataSource.DataSet.GotoBookMark(cb);
  DataSource.DataSet.FreeBookMark(cb);
  DataSource.DataSet.EnableControls;

  EndUpdate;
end;

procedure TDBAdvSmoothTimeLine.OnDataBindingChanged(Sender: TObject);
begin

end;

procedure TDBAdvSmoothTimeLine.SetDataBinding(
  const Value: TDBAdvSmoothTimeLineDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TDBAdvSmoothTimeLine.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

{ TDBAdvSmoothTimeLineDataLink }

procedure TDBAdvSmoothTimeLineDataLink.ActiveChanged;
begin
  inherited;
  FTimeLine.LoadData;
end;

constructor TDBAdvSmoothTimeLineDataLink.Create(ATimeLine: TDBAdvSmoothTimeLine);
begin
  FTimeLine := ATimeLine;
end;

procedure TDBAdvSmoothTimeLineDataLink.DataSetChanged;
begin
  inherited;
  if (DataSet.State = dsBrowse) and FEditChange then
  begin
    FEditChange := false;
  end;

  if (DataSet.State = dsBrowse) {and FEditUpdate }and not FLoading then
  begin
    FEditUpdate := false;
    // reload here after editing was done...
    FLoading := true;
    FTimeLine.LoadData;
    FLoading := false;
  end;
end;

procedure TDBAdvSmoothTimeLineDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;

end;

destructor TDBAdvSmoothTimeLineDataLink.Destroy;
begin

  inherited;
end;

procedure TDBAdvSmoothTimeLineDataLink.EditingChanged;
begin
  FEditChange := true;
  inherited;
end;

procedure TDBAdvSmoothTimeLineDataLink.LayoutChanged;
begin
  inherited;

end;

procedure TDBAdvSmoothTimeLineDataLink.Modified;
begin

end;

procedure TDBAdvSmoothTimeLineDataLink.RecordChanged(Field: TField);
begin
  inherited;
end;

procedure TDBAdvSmoothTimeLineDataLink.Reset;
begin

end;

procedure TDBAdvSmoothTimeLineDataLink.UpdateData;
begin
  FEditUpdate := true;
  inherited;
end;

{ TDBAdvSmoothTimeLineDataBinding }

procedure TDBAdvSmoothTimeLineDataBinding.Changed;
begin
  FOwner.LoadData;
end;

constructor TDBAdvSmoothTimeLineDataBinding.Create(
  AOwner: TDBAdvSmoothTimeLine);
begin
  FOwner := AOwner;
  FIndicatorPicture := '';
  FSectionFixedSize := '';
  FSectionEndTime := '';
  FSectionCaption := '';
  FIndicatorShape := '';
  FSectionFixedPosition := '';
  FIndicatorTime := '';
  FSectionHint := '';
  FIndicatorFixed := '';
  FSectionStartTime := '';
  FIndicatorAnnotation := '';
  FKey := '';
  FSectionColor := '';
  FIndicatorColor := '';
  FIndicatorAnnotationColor := '';
  FIndicatorAnnotationPosition := '';
  FSectionColorTo := '';
  FIndicatorColorTo := '';
  FIndicatorAnnotationColorTo := '';
  FSectionColorMirror := '';
  FSectionColorMirrorTo := '';
  FIndicatorAnnotationTextColor := '';
end;

destructor TDBAdvSmoothTimeLineDataBinding.Destroy;
begin

  inherited;
end;

function TDBAdvSmoothTimeLineDataBinding.GetDBTimeLine: TDBAdvSmoothTimeLine;
begin
  Result := FOwner;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorAnnotationTextColor(
  const Value: String);
begin
  if FIndicatorAnnotationTextColor <> value then
  begin
    FIndicatorAnnotationTextColor := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorAnnotation(
  const Value: String);
begin
  if FIndicatorAnnotation <> Value then
  begin
    FIndicatorAnnotation := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorAnnotationColor(
  const Value: String);
begin
  if FIndicatorAnnotationColor <> value then
  begin
    FIndicatorAnnotationColor := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorAnnotationImageIndex(
  const Value: String);
begin
  if FIndicatorAnnotationImageIndex <> Value then
  begin
    FIndicatorAnnotationImageIndex := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorAnnotationPosition(
  const Value: String);
begin
  if FIndicatorAnnotationPosition <> Value then
  begin
    FIndicatorAnnotationPosition := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorColor(
  const Value: String);
begin
  if FIndicatorColor <> Value then
  begin
    FIndicatorColor := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorColorTo(
  const Value: String);
begin
  if FIndicatorColorTo <> Value then
  begin
    FIndicatorColorTo := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorFixed(
  const Value: string);
begin
  if FIndicatorFixed <> value then
  begin
    FIndicatorFixed := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorPicture(
  const Value: string);
begin
  if FIndicatorPicture <> Value then
  begin
    FIndicatorPicture := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorShape(
  const Value: string);
begin
  if FIndicatorShape <> value then
  begin
    FIndicatorShape := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetIndicatorTime(const Value: string);
begin
  if FIndicatorTime <> value then
  begin
    FIndicatorTime := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetKey(const Value: String);
begin
  if FKey <> value then
  begin
    FKey := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionCaption(
  const Value: string);
begin
  if FSectionCaption <> value then
  begin
    FSectionCaption := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionColor(const Value: String);
begin
  if FSectionColor <> value then
  begin
    FSectionColor := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionColorMirror(
  const Value: String);
begin
  if FSectionColorMirror <> value then
  begin
    FSectionColorMirror := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionColorMirrorTo(
  const Value: String);
begin
  if FSectionColorMirrorTo <> Value then
  begin
    FSectionColorMirrorTo := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionColorTo(
  const Value: String);
begin
  if FSectionColorTo <> value then
  begin
    FSectionColorTo := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionEndTime(
  const Value: string);
begin
  if FSectionEndTime <> value then
  begin
    FSectionEndTime := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionFixedPosition(
  const Value: string);
begin
  if FSectionFixedPosition <> value then
  begin
    FSectionFixedPosition := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionFixedSize(
  const Value: string);
begin
  if FSectionFixedSize <> value then
  begin
    FSectionFixedSize := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionHint(const Value: string);
begin
  if FSectionHint <> value then
  begin
    FSectionHint := Value;
    Changed;
  end;
end;

procedure TDBAdvSmoothTimeLineDataBinding.SetSectionStartTime(
  const Value: string);
begin
  if FSectionStartTime <> Value then
  begin
    FSectionStartTime := Value;
    Changed;
  end;
end;

end.
