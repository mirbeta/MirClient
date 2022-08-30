{***************************************************************************}
{ Property preferences storage                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2003 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit AdvTrackBarPersist;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, TypInfo, INIFiles, Dialogs;


type
  TPropertyPersister = class(TComponent)
  private
    FIgnoreProperties: TStrings;
    FIgnoreSubProperties: TStrings;    
    FComponent: TComponent;
    procedure SetIgnoreProperties(const Value: TStrings);
    procedure SetIgnoreSubProperties(const Value: TStrings);    
  protected
    procedure LoadRTTIComponent(AReader: TReader; const Value: TPersistent; Position: Boolean);
    procedure SaveRTTIComponent(AWriter: TWriter; const Value: TPersistent; Position: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure IniLoadRTTIComponent(AINIFile: TIniFile; const Value: TPersistent; Position: Boolean);
    procedure IniSaveRTTIComponent(AINIFile: TIniFile; const Value: TPersistent; Position: Boolean);

    procedure StoreProperties(AComponent: TComponent; AStream: TStream; Position: Boolean);
    procedure RestoreProperties(AComponent:TComponent; AStream: TStream; Position: Boolean);

    procedure StorePropertiesToFile(AComponent: TComponent; FileName: string);
    procedure RestorePropertiesToFile(AComponent:TComponent; FileName: string);
  published
    property Component: TComponent read FComponent write FComponent;
    property IgnoreProperties: TStrings read FIgnoreProperties write SetIgnoreProperties;
    property IgnoreSubProperties: TStrings read FIgnoreSubProperties write SetIgnoreSubProperties;    
  end;

implementation

function IsReadAndWriteProp(AInstance: TObject; const APropName:string): Boolean;
begin
  with GetPropInfo(AInstance, APropName)^ do
    Result := (SetProc <> nil) and (GetProc <> nil);
end;

function GetSetPropModified(Instance: TObject; PropInfo: PPropInfo): string;
{Modified GetSetProp routine from TypInfo}
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result:='';
  Integer(S) := GetOrdProp(Instance, PropInfo);
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
end;

{ TPropertyPersister }

procedure TPropertyPersister.SaveRTTIComponent(AWriter: TWriter; const Value: TPersistent; Position: Boolean);

  procedure AddProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;
    ms: TMemoryStream;
    ch: byte;

  begin
    if AObject is TGraphic then
    begin
      ms := TMemoryStream.Create;
      (AObject as TGraphic).SaveToStream(ms);
      AWriter.WriteInteger(ms.Size);
      ms.Position := 0;
      Count := ms.Size;

      if Count > 0 then
      begin
        for i := 0 to Count - 1 do
        begin
          ms.Read(ch, 1);
          AWriter.Write(ch,1);
        end;
      end;

      ms.Free;
      Exit;
    end;

    Count := GetPropList(TypeInfo,tkProperties,nil);

    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);

      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);

        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar('save:'+propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not ((IgnoreSubProperties.IndexOf(AObject.ClassName + '.' + PropName) <> -1)) and
          not ((IgnoreProperties.IndexOf(PropName) <> -1) and (AObject = Value)) then
        begin
          {$IFNDEF DELPHI9_LVL}
          if (AObject is TFont) and (propName = 'Pitch') then
            AWriter.WriteInteger(0);
          {$ENDIF}

          case PropList^[I - 1].PropType^.Kind of
          //tkArray:
          //  outputdebugstring('array property');

          tkFloat:
             AWriter.WriteFloat(GetFloatProp(AObject,PropList^[I - 1]));

          tkInteger,
          tkWChar,
          tkChar:
             AWriter.WriteInteger(GetOrdProp(AObject,PropList^[I - 1]));

          tkString,
          tkLString,
          {$IFDEF DELPHI_UNICODE}
          tkUString,
          {$ENDIF}
          tkWString:
             AWriter.WriteString(GetStrProp(AObject, PropList^[I - 1]));

          tkEnumeration:
            AWriter.WriteString(GetEnumProp(AObject, PropList^[I - 1]));

          tkSet:
            AWriter.WriteString(GetSetPropModified(AObject,PropList^[I - 1]));
          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
             //outputdebugstring('class property');
             SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
             SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));
             if Assigned(SubObject) then
             begin
               AddProps(SubObject,SubClass.ClassInfo,Level + 1);
             end;
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  AWriter.WriteString(Value.ClassName);
  AddProps(Value,Value.ClassInfo,0);
end;


procedure TPropertyPersister.IniSaveRTTIComponent(AIniFile: TIniFile; const Value: TPersistent; Position: Boolean);

  procedure AddProps(AObject: TObject; BasePropName:string; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;
    TypeData : PTypeData;

  begin
    Count := GetPropList(TypeInfo,tkProperties,nil);
    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);

        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar(propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not ((IgnoreSubProperties.IndexOf(AObject.ClassName+ '.' + PropName) <> -1)) and
          not ((IgnoreProperties.IndexOf(PropName) <> -1) and (AObject = Value)) then
        begin
          case PropList^[I - 1].PropType^.Kind of
          tkFloat:
             AIniFile.WriteString(BasePropName + string(TypeInfo.Name) ,PropName,Format('%g',[GetFloatProp(AObject,PropList^[I - 1])]));

          tkInteger,
          tkWChar,
          tkChar:
             AIniFile.WriteInteger(BasePropName + string(TypeInfo.Name) ,PropName,GetOrdProp(AObject,PropList^[I - 1]));

          tkString,
          tkLString,
          {$IFDEF DELPHI_UNICODE}
          tkUString,
          {$ENDIF}
          tkWString:
             AIniFile.WriteString(BasePropName + string(TypeInfo.Name) ,PropName,GetStrProp(AObject, PropList^[I - 1]));

          tkEnumeration:
            begin
              TypeData := GetTypeData(PropList^[I - 1].PropType^);
              AIniFile.WriteString(BasePropName + string(TypeInfo.Name),PropName,GetEnumName(TypeData.BaseType^, GetOrdProp(AObject,string(PropList^[I - 1].Name))));
            end;

          tkSet:
            AIniFile.WriteString(BasePropName + string(TypeInfo.Name),PropName,GetSetPropModified(AObject,PropList^[I - 1]));
          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
             SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
             SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));
             if Assigned(SubObject) then
             begin
               AddProps(SubObject,BasePropName + '_' + PropName, SubClass.ClassInfo,Level + 1);
             end;
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  AddProps(Value,'',Value.ClassInfo,0);  
end;


procedure TPropertyPersister.LoadRTTIComponent(AReader: TReader; const Value: TPersistent; Position: Boolean);

  procedure LoadProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;
    ms: TMemoryStream;
    ch: byte;                                                   
  begin
    if AObject is TGraphic then
    begin
      Count := AReader.ReadInteger;
      if Count > 0 then
      begin
        ms := TMemoryStream.Create;
        for i := 0 to Count - 1 do
        begin
          AReader.Read(ch,1);
          ms.WriteBuffer(ch,1);
        end;

        ms.Position := 0;
        if ms.Size = 0 then
          (AObject as TGraphic).Assign(nil)
        else
          (AObject as TGraphic).LoadFromStream(ms);
        ms.Free;
      end;
      Exit;
    end;

    Count := GetPropList(TypeInfo,tkProperties,nil);

    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);



        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar(AObject.ClassName + ' -> '+propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not ((IgnoreSubProperties.IndexOf(AObject.ClassName + '.' + PropName) <> -1)) and
          not ((IgnoreProperties.IndexOf(PropName) <> -1) and (AObject = Value)) then
        begin
          {$IFDEF TMSDEBUG}
          outputdebugstring(pchar('assign:'+propname + ': '+s));
          {$ENDIF}

          {$IFNDEF DELPHI9_LVL}
          if (AObject is TFont) and (propName = 'Pitch') then
            AReader.ReadInteger;
          {$ENDIF}

          case PropList^[I - 1].PropType^.Kind of
          tkFloat:       SetFloatProp(AObject, PropList^[I - 1],
                         AReader.ReadFloat);

          tkInteger,
          tkChar,
          tkWChar:       SetOrdProp(AObject, PropList^[I - 1],
                         AReader.ReadInteger);
          tkString,
          tkLString,
          {$IFDEF DELPHI_UNICODE}
          tkUString,
          {$ENDIF}
          tkWString:

            begin
                SetStrProp(AObject, PropList^[I - 1],
                AReader.ReadString);
            end;

          tkEnumeration: SetEnumProp(AObject, PropList^[I - 1],
                         AReader.ReadString);

          tkSet:         SetSetProp(AObject, PropList^[I - 1],
                         AReader.ReadString);
          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
            SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
            SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));

            if Assigned(SubObject) then
               LoadProps(SubObject,SubClass.ClassInfo,Level + 1);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  if AReader.ReadString <> Value.ClassName then
    raise Exception.Create('Class preferences of different class')
  else
    LoadProps(Value,Value.ClassInfo,0);
end;

procedure TPropertyPersister.IniLoadRTTIComponent(AIniFile: TIniFile; const Value: TPersistent; Position: Boolean);

  procedure LoadProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;
    CurrentClass:string;
  begin
    Count := GetPropList(TypeInfo,tkProperties,nil);
    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      CurrentClass := string(TypeInfo.Name);
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);

        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar('read:'+propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not ((IgnoreSubProperties.IndexOf(AObject.ClassName+'.'+PropName) <> -1)) and
          not ((IgnoreProperties.IndexOf(PropName) <> -1) and (AObject = Value)) then
        begin
          case PropList^[I - 1].PropType^.Kind of

          tkFloat:
            begin
              s := AINIFile.ReadString(string(TypeInfo.Name), PropName,'0');
              SetFloatProp(AObject, PropList^[I - 1], StrToFloat(s));
            end;

          tkInteger,
          tkChar,
          tkWChar:
             SetOrdProp(AObject, PropList^[I - 1], AINIFile.ReadInteger(string(TypeInfo.Name), PropName,0) );

          tkString,
          tkLString,
          {$IFDEF DELPHI_UNICODE}
          tkUString,
          {$ENDIF}
          tkWString:
            SetStrProp(AObject, PropList^[I - 1], AINIFile.ReadString(string(TypeInfo.Name),PropName,''));

          tkEnumeration: SetEnumProp(AObject, PropList^[I - 1],AINIFile.ReadString(string(TypeInfo.Name),PropName,''));

          tkSet:         SetSetProp(AObject, PropList^[I - 1],AINIFile.ReadString(string(TypeInfo.Name),PropName,''));

          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
             SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
             SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));
             if Assigned(SubObject) then
               LoadProps(SubObject,SubClass.ClassInfo,Level + 1);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  LoadProps(Value,Value.ClassInfo,0);
end;


constructor TPropertyPersister.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIgnoreProperties := TStringList.Create;
  FIgnoreProperties.Add('Align');
  FIgnoreProperties.Add('AlignWithMargins');
  FIgnoreProperties.Add('Anchors');
  FIgnoreProperties.Add('Top');
  FIgnoreProperties.Add('Left');
  FIgnoreProperties.Add('Margins');
  FIgnoreProperties.Add('Constraints');
  FIgnoreProperties.Add('DragMode');
  FIgnoreProperties.Add('DragKind');
  FIgnoreProperties.Add('DragCursor');
  FIgnoreProperties.Add('HelpContext');
  FIgnoreProperties.Add('HelpKeyword');
  FIgnoreProperties.Add('HelpType');
  FIgnoreProperties.Add('Visible');
  FIgnoreProperties.Add('Hint');
  FIgnoreProperties.Add('ShowHint');
  FIgnoreProperties.Add('Caption');
  FIgnoreProperties.Add('Width');
  FIgnoreProperties.Add('Height');
  FIgnoreProperties.Add('Tag');
  FIgnoreProperties.Add('TabOrder');
  FIgnoreProperties.Add('TabStop');
  FIgnoreProperties.Add('TickImages');
  FIgnoreProperties.Add('ParentCustomHint');
  FIgnoreProperties.Add('TrackLabel');
  FIgnoreProperties.Add('MoveOnClick');
  FIgnoreProperties.Add('Version');  
  FIgnoreSubProperties := TStringList.Create;
  FIgnoreSubProperties.Add('TTrackBarButtons.Step');
  FIgnoreSubProperties.Add('TTrackBarButtons.RotateText');
  FIgnoreSubProperties.Add('TTrackBarThumb.ThumbPointer');
  FIgnoreSubProperties.Add('TFont.Quality');

end;

destructor TPropertyPersister.Destroy;
begin
  FIgnoreProperties.Free;
  FIgnoreSubProperties.Free;
  inherited;
end;

procedure TPropertyPersister.RestoreProperties(AComponent: TComponent;
  AStream: TStream; Position: Boolean);
var
  reader: TReader;
begin
  reader := TReader.Create(AStream, $FF);
  try
    reader.ReadListBegin;
    LoadRTTIComponent(reader,AComponent, Position);
    reader.ReadListEnd;
  finally
    reader.Free;
  end;
end;

procedure TPropertyPersister.RestorePropertiesToFile(
  AComponent: TComponent; FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(FileName);
  ReStoreProperties(AComponent,ms,false);
  ms.Free;
end;

procedure TPropertyPersister.SetIgnoreProperties(const Value: TStrings);
begin
  FIgnoreProperties.Assign(Value);
end;

procedure TPropertyPersister.SetIgnoreSubProperties(const Value: TStrings);
begin
  FIgnoreSubProperties.Assign(Value);
end;

procedure TPropertyPersister.StoreProperties(AComponent: TComponent;
  AStream: TStream; Position: Boolean);
var
  writer: TWriter;
begin
  writer := TWriter.Create(AStream, $FF);
  try
    writer.WriteListBegin;
    SaveRTTIComponent(writer,AComponent, Position);
    writer.WriteListEnd;
  finally
    writer.Free;
  end;
end;

procedure TPropertyPersister.StorePropertiesToFile(AComponent: TComponent;
  FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  StoreProperties(AComponent,ms,false);
  ms.SaveToFile(FileName);
  ms.Free;
end;

end.


