unit InPlaceEditorsDemoFrameManager;

interface

uses
  Windows, Classes, Controls, SysUtils, Graphics, Forms, StdCtrls, ExtCtrls;

type
  TEditorDemoBaseFrame = class(TForm)
    lblFrameDescription: TLabel;
  private
    function GetDescriptionVisible: Boolean;
    procedure SetDescriptionVisible(AValue: Boolean);
  public
    procedure SetDescription(ADescription: string);

    property DescriptionVisible: Boolean read GetDescriptionVisible write SetDescriptionVisible;
  end;

  TEditorDemoFrameManager = class
  private
    FFrameList: TList;
    function GetFrame(AIndex: Integer): TEditorDemoBaseFrame;
    function GetFramesCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddFrame(AFrame: TForm; AFrameId: Integer);
    procedure UpdateFrameColors(AColor: TColor);
    procedure SetDescriptionsVisible(AValue: Boolean);

    property FramesCount: Integer read GetFramesCount;
    property Frames[AIndex: Integer]: TEditorDemoBaseFrame read GetFrame; default;
  end;

procedure DrawText(AStrings: array of string; APaintBox: TPaintBox);

var
  AFrameManager: TEditorDemoFrameManager;

implementation

{$R *.dfm}

uses
  Types, Math, cxGraphics, cxGeometry;

procedure DrawText(AStrings: array of string; APaintBox: TPaintBox);
var
  I: Integer;
  S: string;
  ARect: TRect;
  ASize: TSize;
begin
  S := '';
  ASize.cx := 0;
  for I := Low(AStrings) to High(AStrings) do
  begin
    ASize.cx := Max(ASize.cx, APaintBox.Canvas.TextWidth(AStrings[I]));
    S := S + AStrings[I] + #13#10;
  end;

  ASize.cy := APaintBox.Canvas.TextHeight('Wg') * Length(AStrings);

  ARect := cxRectCenter(APaintBox.ClientRect, ASize);
  cxDrawText(APaintBox.Canvas.Handle, S, ARect, DT_WORDBREAK);
end;

{ TEditorDemoBaseFrame }

procedure TEditorDemoBaseFrame.SetDescription(ADescription: string);
begin
  lblFrameDescription.Caption := ADescription;
end;

function TEditorDemoBaseFrame.GetDescriptionVisible: Boolean;
begin
  Result := lblFrameDescription.Visible;
end;

procedure TEditorDemoBaseFrame.SetDescriptionVisible(AValue: Boolean);
begin
  lblFrameDescription.Visible := AValue;
end;

{ TEditorDemoFrameManager }

constructor TEditorDemoFrameManager.Create;
begin
  FFrameList := TList.Create;
end;

destructor TEditorDemoFrameManager.Destroy;
begin
  FreeAndNil(FFrameList);
  inherited;
end;

procedure TEditorDemoFrameManager.AddFrame(AFrame: TForm; AFrameId: Integer);
begin
  FFrameList.Add(AFrame);
  AFrame.Tag := AFrameId;
end;

function TEditorDemoFrameManager.GetFrame(
  AIndex: Integer): TEditorDemoBaseFrame;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FramesCount - 1 do
    if TEditorDemoBaseFrame(FFrameList[I]).Tag = AIndex then
    begin
      Result := TEditorDemoBaseFrame(FFrameList[I]);
      Break;
    end;
end;

function TEditorDemoFrameManager.GetFramesCount: Integer;
begin
  Result := FFrameList.Count;
end;

procedure TEditorDemoFrameManager.UpdateFrameColors(AColor: TColor);
var
  I: Integer;
begin
  for I := 0 to FFrameList.Count - 1 do
    Frames[I].Color := AColor;
end;

procedure TEditorDemoFrameManager.SetDescriptionsVisible(AValue: Boolean);
var
  I: Integer;
begin
  for I := 0 to FramesCount - 1 do
    Frames[I].SetDescriptionVisible(AValue);
end;

initialization
  AFrameManager := TEditorDemoFrameManager.Create;

finalization
  AFrameManager.Free;
end.

