{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxSchedulerICalendar;

{$I cxVer.inc}

interface

uses
  Types, Classes, Windows, Contnrs, dxCore,
  cxDateUtils, cxSchedulerStorage, cxSchedulerCustomControls, cxSchedulerUtils,
  cxSchedulerRecurrence;

type
  TcxSchedulerICalendar = class;

  TcxSchedulerICalendarCustomObject = class;
  TcxSchedulerICalendarCustomProperty = class;
  TcxSchedulerICalendarCustomPropertyClass = class of TcxSchedulerICalendarCustomProperty;
  TcxSchedulerICalendarStorageObject = class;

  { TcxSchedulerICalendarCustomPropertyParameter }

  TcxSchedulerICalendarCustomParameter = class
  protected
    class function ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      var AValue: Variant): Boolean; virtual;
    class function ConvertPropertyFromParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      const AParameterValue: string; var AValue: Variant): Boolean; virtual;

    class function GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string; virtual;
    class function TokenName: string; virtual;

    class function FromString(AProperty: TcxSchedulerICalendarCustomProperty;
      const ATranslateValue: string; out AValue: Variant): Boolean; virtual;
    class function ToString(AProperty: TcxSchedulerICalendarCustomProperty;
      AValue: Variant; out ATranslateValue: string): Boolean; reintroduce; virtual;
  end;
  TcxSchedulerICalendarCustomParameterClass = class of TcxSchedulerICalendarCustomParameter;

  { TcxSchedulerICalendarCustomProperty }

  TcxSchedulerICalendarCustomProperty = class
  private
    FICalendar: TcxSchedulerICalendar;
    FCustomParameters: TList;
    FParent: TcxSchedulerICalendarCustomObject;
    FSchedulerObject: TObject;
    FValue: Variant;
    function GetCustomParameterCount: Integer;
    function GetCustomParameter(Index: Integer): TcxSchedulerICalendarCustomParameterClass;
  protected
    class function IsBegin(const S: string): Boolean; virtual;
    class function IsEnd(const S: string): Boolean; virtual;

    class function SchedulerObjectClass: TClass; virtual;
    class function TokenName: string; virtual;

    procedure AfterExport(AStrings: TStringList); virtual;
    procedure AfterImport(AStrings: TStringList); virtual;
    procedure BeforeExport(AStrings: TStringList); virtual;
    procedure BeforeImport(AStrings: TStringList); virtual;
    function CanExport: Boolean; virtual;
    function CanImport: Boolean; virtual;
    procedure DoExport(AStrings: TStringList); virtual;
    procedure DoImport(AStrings: TStringList); virtual;

    function FromString(const AValue, AParameters: string): Variant; virtual;
    function ToString: string; reintroduce; virtual;

    procedure Calculate; virtual;
    procedure CreateSchedulerObject;
    procedure DoCreateSchedulerObject; virtual;

    procedure AddCustomParameter(AClass: TcxSchedulerICalendarCustomParameterClass);
    procedure RegisterCustomParameters; virtual;

    function GetValue: Variant; virtual;
    procedure SetValue(const AValue: Variant); virtual;

    property CustomParameterCount: Integer read GetCustomParameterCount;
    property CustomParameters[Index: Integer]: TcxSchedulerICalendarCustomParameterClass read GetCustomParameter;
    property SchedulerObject: TObject read FSchedulerObject;
    property Value: Variant read GetValue write SetValue;
  public
    constructor Create(AICalendar: TcxSchedulerICalendar; AParent: TcxSchedulerICalendarCustomObject); virtual;
    destructor Destroy; override;

    procedure Export(AStrings: TStringList);
    procedure Import(AStrings: TStringList);

    class procedure Register;
    class procedure Unregister;

    property ICalendar: TcxSchedulerICalendar read FICalendar;
    property Parent: TcxSchedulerICalendarCustomObject read FParent;
  end;

  { TcxSchedulerICalendarCustomObject }

  TcxSchedulerICalendarCustomObject = class(TcxSchedulerICalendarCustomProperty)
  private
    FProperties: TList;
    function GetProperty(Index: Integer): TcxSchedulerICalendarCustomPropertyClass;
    function GetPropertyCount: Integer;
  protected
    class function IsBegin(const S: string): Boolean; override;
    class function IsEnd(const S: string): Boolean; override;

    procedure AfterExport(AStrings: TStringList); override;
    procedure BeforeExport(AStrings: TStringList); override;
    function CanExport: Boolean; override;
    function CanImport: Boolean; override;
    procedure DoExport(AStrings: TStringList); override;
    procedure DoImport(AStrings: TStringList); override;

    procedure AddProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass);
    function CanHaveProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass): Boolean; virtual;

    procedure PopulateProperties; virtual;

    property Properties[Index: Integer]: TcxSchedulerICalendarCustomPropertyClass read GetProperty;
    property PropertyCount: Integer read GetPropertyCount;
  public
    constructor Create(AICalendar: TcxSchedulerICalendar; AParent: TcxSchedulerICalendarCustomObject); override;
    destructor Destroy; override;
  end;
  TcxSchedulerICalendarCustomObjectClass = class of TcxSchedulerICalendarCustomObject;

  { Strings }

  TcxSchedulerICalendarLanguageParameter = class(TcxSchedulerICalendarCustomParameter)
  protected
    class function ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      var AValue: Variant): Boolean; override;
    class function ConvertPropertyFromParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      const AParameterValue: string; var AValue: Variant): Boolean; overload; override;
    class function GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string; override;
    class function TokenName: string; override;
  end;

  TcxSchedulerICalendarStringParameter = class(TcxSchedulerICalendarCustomParameter)
  protected
    class function FromString(AProperty: TcxSchedulerICalendarCustomProperty;
      const ATranslateValue: string; out AValue: Variant): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarCustomProperty;
      AValue: Variant; out ATranslateValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarStringCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  protected
    procedure RegisterCustomParameters; override;
  end;

  { Recurrence }

  TcxSchedulerICalendarRecurrenceCustomProperty = class;

  TcxSchedulerICalendarRecurrenceCustomValue = class
  protected
    class function TokenName: string; virtual;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; virtual;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; reintroduce; virtual;
  end;
  TcxSchedulerICalendarRecurrenceCustomValueClass = class of TcxSchedulerICalendarRecurrenceCustomValue;

  TcxSchedulerICalendarRecurrenceTypeValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceCountValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceUntilValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceIntervalValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceByDayValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceByMonthValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceByMonthDayValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceBySetPosValue = class(TcxSchedulerICalendarRecurrenceCustomValue)
  protected
    class function TokenName: string; override;
    class function FromString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarRecurrenceCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    FValueParameters: TList;
    function GetValueParameter(Index: Integer): TcxSchedulerICalendarRecurrenceCustomValueClass;
    function GetValueParameterCount: Integer;
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure AddValueParameter(AParameter: TcxSchedulerICalendarRecurrenceCustomValueClass);
    procedure PopulateValueParameters;

    function GetByDay: TDays; virtual; abstract;
    function GetByDayWeek(ADay: TDay): Integer; virtual;
    function GetByMonth: Integer; virtual; abstract;
    function GetByMonthDay: Integer; virtual; abstract;
    function GetBySetPos: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetInterval: Integer; virtual; abstract;
    function GetRecurrenceType: TcxRecurrence; virtual; abstract;
    function GetRecurrenceUntil: TDateTime; virtual; abstract;
    procedure SetByDay(AValue: TDays); virtual; abstract;
    procedure SetByDayWeek(ADay: TDay; AValue: Integer); virtual;
    procedure SetByMonth(AValue: Integer); virtual; abstract;
    procedure SetByMonthDay(AValue: Integer); virtual; abstract;
    procedure SetBySetPos(AValue: Integer); virtual; abstract;
    procedure SetCount(AValue: Integer); virtual; abstract;
    procedure SetInterval(AValue: Integer); virtual; abstract;
    procedure SetRecurrenceType(AValue: TcxRecurrence); virtual; abstract;
    procedure SetRecurrenceUntil(AValue: TDateTime); virtual; abstract;

    property ValueParameters[Index: Integer]: TcxSchedulerICalendarRecurrenceCustomValueClass read GetValueParameter;
    property ValueParameterCount: Integer read GetValueParameterCount;
  public
    constructor Create(AICalendar: TcxSchedulerICalendar; AParent: TcxSchedulerICalendarCustomObject); override;
    destructor Destroy; override;

    property ByDay: TDays read GetByDay write SetByDay;
    property ByDayWeek[ADay: TDay]: Integer read GetByDayWeek write SetByDayWeek;
    property ByMonth: Integer read GetByMonth write SetByMonth;
    property ByMonthDay: Integer read GetByMonthDay write SetByMonthDay;
    property BySetPos: Integer read GetBySetPos write SetBySetPos;
    property Count: Integer read GetCount write SetCount;
    property Interval: Integer read GetInterval write SetInterval;
    property RecurrenceType: TcxRecurrence read GetRecurrenceType write SetRecurrenceType;
    property RecurrenceUntil: TDateTime read GetRecurrenceUntil write SetRecurrenceUntil;
  end;

  { DateTime }

  TcxSchedulerICalendarTZIDParameter = class(TcxSchedulerICalendarCustomParameter)
  protected
    class function ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      var AValue: Variant): Boolean; override;
    class function ConvertPropertyFromParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      const AParameterValue: string; var AValue: Variant): Boolean; overload; override;
    class function GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string; override;
    class function TokenName: string; override;
  end;

  TcxSchedulerICalendarDateParameter = class(TcxSchedulerICalendarCustomParameter)
  protected
    class function ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      var AValue: Variant): Boolean; override;
    class function GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string; override;
    class function TokenName: string; override;

    class function FromString(AProperty: TcxSchedulerICalendarCustomProperty;
      const ATranslateValue: string; out AValue: Variant): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarCustomProperty; AValue: Variant;
      out ATranslateValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarDateTimeParameter = class(TcxSchedulerICalendarDateParameter)
  protected
    class function ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
      var AValue: Variant): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarCustomProperty; AValue: Variant;
      out ATranslateValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarDateTimeCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    FTimeZone: Integer;
    FUseDateFormat: Boolean;
  protected
    procedure RegisterCustomParameters; override;
    function UseTimeZone: Boolean; virtual;

    property TimeZone: Integer read FTimeZone write FTimeZone;
    property UseDateFormat: Boolean read FUseDateFormat write FUseDateFormat;
  end;

  TcxSchedulerICalendarRecurrenceUntilProperty = class(TcxSchedulerICalendarDateTimeCustomProperty)
  protected
    function UseTimeZone: Boolean; override;
  end;

  { TimeZone }

  TcxSchedulerICalendarTimeZoneObject = class;

  TcxSchedulerICalendarTZIDProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    function GetParent: TcxSchedulerICalendarTimeZoneObject;
  protected
    class function TokenName: string; override;
    procedure RegisterCustomParameters; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    property Parent: TcxSchedulerICalendarTimeZoneObject read GetParent;
  end;

  { TimeZone Rules }

  TcxSchedulerICalendarTimeZoneCustomRule = class;

  TcxSchedulerICalendarTimeZoneStartProperty = class(TcxSchedulerICalendarDateTimeCustomProperty)
  private
    function GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
  protected
    procedure Calculate; override;
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    property Parent: TcxSchedulerICalendarTimeZoneCustomRule read GetParent;
  end;

  TcxSchedulerICalendarTimeZoneRuleCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    function GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
  protected
    function GetCurrentTimeZone: Integer;
    function GetTimeZoneInformation: TcxTimeZoneInformation;

    function FromUtcOffset(const AValue: TDateTime): string;
  public
    property Parent: TcxSchedulerICalendarTimeZoneCustomRule read GetParent;
  end;

  TcxSchedulerICalendarTZOffsetToProperty = class(TcxSchedulerICalendarTimeZoneRuleCustomProperty)
  protected
    class function TokenName: string; override;
    function GetBiasFromStr(const AValue: string): Integer;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarTZOffsetFromProperty = class(TcxSchedulerICalendarTimeZoneRuleCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarTimeZoneRuleRecurrence = class(TcxSchedulerICalendarRecurrenceCustomProperty)
  private
    function GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
  protected
    function CanExport: Boolean; override;

    function GetByDay: TDays; override;
    function GetByDayWeek(ADay: TDay): Integer; override;
    function GetByMonth: Integer; override;
    function GetByMonthDay: Integer; override;
    function GetBySetPos: Integer; override;
    function GetCount: Integer; override;
    function GetInterval: Integer; override;
    function GetRecurrenceType: TcxRecurrence; override;
    function GetRecurrenceUntil: TDateTime; override;
    procedure SetByDay(AValue: TDays); override;
    procedure SetByDayWeek(ADay: TDay; AValue: Integer); override;
    procedure SetByMonth(AValue: Integer); override;
    procedure SetByMonthDay(AValue: Integer); override;
    procedure SetBySetPos(AValue: Integer); override;
    procedure SetCount(AValue: Integer); override;
    procedure SetInterval(AValue: Integer); override;
    procedure SetRecurrenceType(AValue: TcxRecurrence); override;
    procedure SetRecurrenceUntil(AValue: TDateTime); override;

    property Parent: TcxSchedulerICalendarTimeZoneCustomRule read GetParent;
  end;

  TcxSchedulerICalendarTimeZoneCustomRule = class(TcxSchedulerICalendarCustomObject)
  private
    FBias: TDateTime;
    FUtcOffset: TDateTime;
    function GetParent: TcxSchedulerICalendarTimeZoneObject;
  protected
    procedure Calculate; override;
    function CanExport: Boolean; override;
    procedure PopulateProperties; override;

    function GetDayOfWeek: TDay;
    function IsStandardRule: Boolean; virtual;
    function GetStart: TDateTime;
  public
    property Parent: TcxSchedulerICalendarTimeZoneObject read GetParent;
  end;

  TcxSchedulerICalendarTimeZoneStandardRule = class(TcxSchedulerICalendarTimeZoneCustomRule)
  protected
    class function TokenName: string; override;
  end;

  TcxSchedulerICalendarTimeZoneDaylightRule = class(TcxSchedulerICalendarTimeZoneCustomRule)
  protected
    class function TokenName: string; override;
    function IsStandardRule: Boolean; override;
  end;

  { TcxSchedulerICalendarTimeZoneObject }

  TcxSchedulerICalendarTimeZoneObject = class(TcxSchedulerICalendarCustomObject)
  private
    function InternalSystemTimeToDateTime(ADate: TSystemTime): TDateTime;
  protected
    FInternalTimeZone: TcxTimeZoneInformation;

    class function TokenName: string; override;
    procedure PopulateProperties; override;

    procedure AfterImport(AStrings: TStringList); override;

    function GetDaylightDate: TDateTime;
    function GetStandartDate: TDateTime;

    function GetDaylightDayOfWeek: TDay;
    function GetStandardDayOfWeek: TDay;

    function GetCurrentTimeZone: Integer;
    function GetTimeZoneInformation: TcxTimeZoneInformation;
    function UseOnlyStandardTime: Boolean;
  end;

  { Events}

  TcxSchedulerICalendarEventObject = class;

  TcxSchedulerICalendarEventDateTimeProperty = class(TcxSchedulerICalendarDateTimeCustomProperty)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetParent: TcxSchedulerICalendarEventObject;
  protected
    procedure Calculate; override;

    property Event: TcxSchedulerEvent read GetEvent;
    property Parent: TcxSchedulerICalendarEventObject read GetParent;
  end;

  TcxSchedulerICalendarEventStampProperty = class(TcxSchedulerICalendarEventDateTimeProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarEventStartProperty = class(TcxSchedulerICalendarEventDateTimeProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventEndProperty = class(TcxSchedulerICalendarEventDateTimeProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetParent: TcxSchedulerICalendarEventObject;
  protected
    property Event: TcxSchedulerEvent read GetEvent;
    property Parent: TcxSchedulerICalendarEventObject read GetParent;
  end;

  TcxSchedulerICalendarEventUIDProperty = class(TcxSchedulerICalendarEventCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarEventRecurrenceProperty = class(TcxSchedulerICalendarRecurrenceCustomProperty)
  private
    function GetParent: TcxSchedulerICalendarEventObject;
    function GetRecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
  protected
    function CanExport: Boolean; override;
    procedure SetValue(const AValue: Variant); override;

    function GetEvent: TcxSchedulerEvent; virtual;

    function GetByDay: TDays; override;
    function GetByMonth: Integer; override;
    function GetByMonthDay: Integer; override;
    function GetBySetPos: Integer; override;
    function GetCount: Integer; override;
    function GetInterval: Integer; override;
    function GetRecurrenceType: TcxRecurrence; override;
    function GetRecurrenceUntil: TDateTime; override;
    procedure SetByDay(AValue: TDays); override;
    procedure SetByDayWeek(ADay: TDay; AValue: Integer); override;
    procedure SetByMonth(AValue: Integer); override;
    procedure SetByMonthDay(AValue: Integer); override;
    procedure SetBySetPos(AValue: Integer); override;
    procedure SetCount(AValue: Integer); override;
    procedure SetInterval(AValue: Integer); override;
    procedure SetRecurrenceType(AValue: TcxRecurrence); override;
    procedure SetRecurrenceUntil(AValue: TDateTime); override;

    property Event: TcxSchedulerEvent read GetEvent;
    property Parent: TcxSchedulerICalendarEventObject read GetParent;
    property RecurrenceInfo: TcxSchedulerEventRecurrenceInfo read GetRecurrenceInfo;
  end;

  TcxSchedulerICalendarEventStringProperty = class(TcxSchedulerICalendarStringCustomProperty)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetParent: TcxSchedulerICalendarEventObject;
  protected
    property Event: TcxSchedulerEvent read GetEvent;
    property Parent: TcxSchedulerICalendarEventObject read GetParent;
  end;

  TcxSchedulerICalendarEventDescriptionProperty = class(TcxSchedulerICalendarEventStringProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventLocationProperty = class(TcxSchedulerICalendarEventStringProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventSummaryProperty = class(TcxSchedulerICalendarEventStringProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventStatusProperty = class(TcxSchedulerICalendarEventCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventLabelProperty = class(TcxSchedulerICalendarEventCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarEventResourcesProperty = class(TcxSchedulerICalendarEventStringProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  { Alarm }

  TcxSchedulerICalendarAlarmObject = class;

  TcxSchedulerICalendarAlarmCustomProperty = class(TcxSchedulerICalendarCustomProperty)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetParent: TcxSchedulerICalendarAlarmObject;
  protected
    property Event: TcxSchedulerEvent read GetEvent;
    property Parent: TcxSchedulerICalendarAlarmObject read GetParent;
  end;

  TcxSchedulerICalendarAlarmTriggerParameter = class(TcxSchedulerICalendarCustomParameter)
  protected
    class function FromString(AProperty: TcxSchedulerICalendarCustomProperty;
      const ATranslateValue: string; out AValue: Variant): Boolean; override;
    class function ToString(AProperty: TcxSchedulerICalendarCustomProperty;
      AValue: Variant; out ATranslateValue: string): Boolean; override;
  end;

  TcxSchedulerICalendarAlarmTriggerProperty = class(TcxSchedulerICalendarAlarmCustomProperty)
  protected
    class function TokenName: string; override;
    procedure RegisterCustomParameters; override;

    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  end;

  TcxSchedulerICalendarAlarmActionProperty = class(TcxSchedulerICalendarAlarmCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarAlarmObject = class(TcxSchedulerICalendarCustomObject)
  private
    function GetParent: TcxSchedulerICalendarEventObject;
  protected
    class function TokenName: string; override;
    function CanExport: Boolean; override;
    procedure PopulateProperties; override;
    property Parent: TcxSchedulerICalendarEventObject read GetParent;
  end;

  { TcxSchedulerICalendarEventObject }

  TcxSchedulerICalendarEventObject = class(TcxSchedulerICalendarCustomObject)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetIsAllDayEvent: Boolean;
    procedure SetIsAllDayEvent(Value: Boolean);
    function GetParent: TcxSchedulerICalendarStorageObject;
  protected
    class function TokenName: string; override;
    class function SchedulerObjectClass: TClass; override;

    procedure AfterImport(AStrings: TStringList); override;
    procedure DoCreateSchedulerObject; override;

    procedure PopulateProperties; override;

    property Event: TcxSchedulerEvent read GetEvent;
    property IsAllDayEvent: Boolean read GetIsAllDayEvent write SetIsAllDayEvent;
    property Parent: TcxSchedulerICalendarStorageObject read GetParent;
  end;

  { TcxSchedulerICalendarCalendarObject }

  TcxSchedulerICalendarVersionProperty = class(TcxSchedulerICalendarCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarProdIDProperty = class(TcxSchedulerICalendarCustomProperty)
  protected
    class function TokenName: string; override;
    function GetValue: Variant; override;
  end;

  TcxSchedulerICalendarStorageObject = class(TcxSchedulerICalendarCustomObject)
  private
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    class function TokenName: string; override;
    class function SchedulerObjectClass: TClass; override;

    procedure DoExport(AStrings: TStringList); override;

    function CanHaveProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass): Boolean; override;
    procedure PopulateProperties; override;

    procedure DoCreateSchedulerObject; override;

    property Storage: TcxCustomSchedulerStorage read GetStorage;
  end;

  { TcxSchedulerICalendar }

  TcxSchedulerICalendar = class(TPersistent)
  private
    FDateTime: TDateTime;
    FTimeZone: Integer;
    FStorage: TcxCustomSchedulerStorage;
  protected
    function CanExport(AObject: TcxSchedulerICalendarCustomObject): Boolean; virtual;
    function CanImport(AObject: TcxSchedulerICalendarCustomObject): Boolean; virtual;
    procedure ExportObject(AObject: TObject; AParent: TcxSchedulerICalendarCustomObject; AStrings: TStringList);
    procedure ImportObject(AParent: TcxSchedulerICalendarCustomObject; AStrings: TStringList);

    function FindTimeZone(const AName: string): Integer;
    function GetStorageTimeZone: Integer;

    property TimeZone: Integer read FTimeZone write FTimeZone;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage);

    procedure Export(const AFileName: string);
    procedure Import(const AFileName: string);

    property Storage: TcxCustomSchedulerStorage read FStorage;
  end;

const
  cxSchedulerICalendarProdID: string = '';

procedure cxSchedulerICalendarExport(AStorage: TcxCustomSchedulerStorage; const AFileName: string);
procedure cxSchedulerICalendarImport(AStorage: TcxCustomSchedulerStorage; const AFileName: string);

implementation

uses
  SysUtils, Forms, StrUtils, Variants, DateUtils, Math, cxClasses, cxVariants;

const
  cxSchedulerICalendarExportVersion = '2.0';
  cxSchedulerICalendarIso8601DateTimeFormat = 'yyyyMMdd"T"HHmmss';
  cxSchedulerICalendarIso8601UtcDateTimeFormat = 'yyyyMMdd"T"HHmmss"Z"';
  cxSchedulerICalendarUtcDateTimePostfix = 'Z';
  cxSchedulerICalendarDateFormat = 'yyyyMMdd';

  cxSchedulerICalendarObjectBeginFormat = 'BEGIN:%s';
  cxSchedulerICalendarObjectEndFormat = 'END:%s';
  cxSchedulerICalendarPropertyFormat = '%s:%s';
  cxSchedulerICalendarPropertyParameterFormat = ';%s=%s';
  cxSchedulerICalendarPropertyValueParameterFormat = '%s=%s';
  cxSchedulerICalendarAfterAlarmTriggerFormat = 'PT%dM';
  cxSchedulerICalendarBeforeAlarmTriggerFormat = '-PT%dM';

  cxSchedulerICalendarValueDelimiter = ':';
  cxSchedulerICalendarParameterDelimiter = ';';
  cxSchedulerICalendarDayDelimiter = ',';
  cxSchedulerICalendarResourceDelimiter = ',';
  cxSchedulerICalendarQuote = '"';

  cxSchedulerICalendarBySetPosInvalidValue = -2;

type
  TcxSchedulerICalendarPropertyManager = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxSchedulerICalendarCustomPropertyClass;
    function GetSchedulerClass(Index: Integer): TClass;
  protected
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxSchedulerICalendarCustomPropertyClass read GetItem;
    property SchedulerClasses[Index: Integer]: TClass read GetSchedulerClass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetICalendarPropertyClass(ASchedulerObject: TObject): TcxSchedulerICalendarCustomPropertyClass; overload;
    function GetICalendarPropertyClass(const S: string;
      AParent: TcxSchedulerICalendarCustomObject = nil): TcxSchedulerICalendarCustomPropertyClass; overload;

    procedure Add(AICalendarPropertyClass: TcxSchedulerICalendarCustomPropertyClass);
    procedure Remove(AICalendarPropertyClass: TcxSchedulerICalendarCustomPropertyClass); overload;
  end;

  { TcxVersionInfo }

  PcxLangAndCodePage = ^TcxLangAndCodePage;
  TcxLangAndCodePage = record
    wLanguage: Word;
    wCodePage: Word;
  end;

  TcxVersionInfo = class
  private
    FComments: string;
    FCompanyName: string;
    FFileVersion: string;
    FFileDescription: string;
    FInternalName: string;
    FLanguage: string;
    FLanguageInfo: string;
    FLegalCopyright: string;
    FProductVersion: string;
    FProductName: string;
    FOriginalFilename: string;
    procedure Build;
  public
    constructor Create;

    property Comments: string read FComments;
    property CompanyName: string read FCompanyName;
    property FileDescription: string read FFileDescription;
    property FileVersion: string read FFileVersion;
    property InternalName: string read FInternalName;
    property Language: string read FLanguage;
    property LanguageInfo: string read FLanguageInfo;
    property LegalCopyright: string read FLegalCopyright;
    property OriginalFilename: string read FOriginalFilename;
    property ProductName: string read FProductName;
    property ProductVersion: string read FProductVersion;
  end;

  TcxSchedulerICalendarDateTimeHelper = class(TcxSchedulerDateTimeHelper);

var
  FObjectManager: TcxSchedulerICalendarPropertyManager;

function cxSchedulerICalendarPropertyManager: TcxSchedulerICalendarPropertyManager;
begin
  if FObjectManager = nil then
    FObjectManager := TcxSchedulerICalendarPropertyManager.Create;
  Result := FObjectManager;
end;

procedure cxSchedulerICalendarExport(AStorage: TcxCustomSchedulerStorage; const AFileName: string);
var
  AICalendar: TcxSchedulerICalendar;
begin
  AICalendar := TcxSchedulerICalendar.Create(AStorage);
  try
    AICalendar.Export(AFileName);
  finally
    AICalendar.Free;
  end;
end;

procedure cxSchedulerICalendarImport(AStorage: TcxCustomSchedulerStorage; const AFileName: string);
var
  AICalendar: TcxSchedulerICalendar;
begin
  AICalendar := TcxSchedulerICalendar.Create(AStorage);
  try
    AICalendar.Import(AFileName);
  finally
    AICalendar.Free;
  end;
end;

function StrSplit(const ASubStr, S: string): TStrings;
begin
  Result := TStringList.Create;
  Result.Text := StringReplace(S, ASubStr, #13#10, [rfReplaceAll]);
end;

function ParameterSplit(const S: string; out AName, AValue: string): Boolean;
var
  APos: Integer;
begin
  APos := Pos('=', S);
  Result := (APos > 0) and (APos < Length(S));
  if Result then
  begin
    AName := Copy(S, 1, APos - 1);
    AValue := S;
    Delete(AValue, 1, APos);
  end;
end;

function ConvertDateToGlobalTime(const ADate: TDateTime; ATimeZone: Integer): TDateTime;
begin
  Result := TcxSchedulerDateTimeHelper.ConvertToGlobalTime(ADate, ATimeZone) +
    TcxSchedulerDateTimeHelper.TimeZoneDaylightBias(ADate, ATimeZone) * MinuteToTime;
end;

function ConvertDateToLocalTime(const ADate: TDateTime; ATimeZone: Integer): TDateTime;
begin
  Result := TcxSchedulerDateTimeHelper.ConvertToLocalTime(ADate, ATimeZone) -
    TcxSchedulerDateTimeHelper.TimeZoneDaylightBias(ADate, ATimeZone) * MinuteToTime;
end;

function ConvertDateToAnotherTimeZone(const ADate: TDateTime; ADateTimeZone, ATimeZone: Integer): TDateTime;
begin
  Result := ConvertDateToGlobalTime(ADate, ADateTimeZone);
  Result := ConvertDateToLocalTime(Result, ATimeZone);
end;

{ TcxSchedulerICalendarPropertyManager }

constructor TcxSchedulerICalendarPropertyManager.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TcxSchedulerICalendarPropertyManager.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TcxSchedulerICalendarPropertyManager.Clear;
begin
  while Count > 0 do
    Remove(Items[0]);
end;

function TcxSchedulerICalendarPropertyManager.GetICalendarPropertyClass(ASchedulerObject: TObject): TcxSchedulerICalendarCustomPropertyClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if ASchedulerObject is SchedulerClasses[I] then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxSchedulerICalendarPropertyManager.GetICalendarPropertyClass(
  const S: string; AParent: TcxSchedulerICalendarCustomObject = nil): TcxSchedulerICalendarCustomPropertyClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].IsBegin(S) and ((AParent = nil) or AParent.CanHaveProperty(Items[I])) then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TcxSchedulerICalendarPropertyManager.Add(AICalendarPropertyClass: TcxSchedulerICalendarCustomPropertyClass);
begin
  FList.Add(AICalendarPropertyClass);
end;

procedure TcxSchedulerICalendarPropertyManager.Remove(AICalendarPropertyClass: TcxSchedulerICalendarCustomPropertyClass);
begin
  FList.Remove(AICalendarPropertyClass);
end;

function TcxSchedulerICalendarPropertyManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxSchedulerICalendarPropertyManager.GetItem(Index: Integer): TcxSchedulerICalendarCustomPropertyClass;
begin
  Result := TcxSchedulerICalendarCustomPropertyClass(FList[Index]);
end;

function TcxSchedulerICalendarPropertyManager.GetSchedulerClass(Index: Integer): TClass;
begin
  Result := Items[Index].SchedulerObjectClass;
end;

{ TcxVersionInfo }

constructor TcxVersionInfo.Create;
begin
  inherited Create;
  Build;
end;

procedure TcxVersionInfo.Build;
var
  ASize: Cardinal;
  ABuf: Pointer;
  ATranslationSize: Cardinal;
  ABufSize: Cardinal;
  ALangAndCodePage: PcxLangAndCodePage;

  function GetStringParam(const AParam: string): PChar;
  var
    AResultSize: Cardinal;
  begin
    VerQueryValue(ABuf, PChar(AParam), Pointer(Result), AResultSize);
    if AResultSize = 0 then
      Result := '';
  end;

begin
  ASize := GetFileVersionInfoSize(PChar(Application.ExeName), ABufSize);
  if ASize > 0 then
  begin
    ABuf := AllocMem(ASize);
    try
      GetFileVersionInfo(PChar(Application.ExeName), 0, ASize, ABuf);
        VerQueryValue(ABuf, '\VarFileInfo\Translation', Pointer(ALangAndCodePage), ATranslationSize);

        if ATranslationSize >= 4 then
        begin
          FLanguageInfo := Format('%4.4x%4.4x', [ALangAndCodePage.wLanguage, ALangAndCodePage.wCodePage]);
          FLanguage := UpperCase(GetLocaleStr(ALangAndCodePage.wLanguage, LOCALE_SISO639LANGNAME, ''));
          FCompanyName := GetStringParam('\StringFileInfo\' + LanguageInfo + '\CompanyName');
          FFileDescription := GetStringParam('\StringFileInfo\' + LanguageInfo + '\FileDescription');
          FFileVersion := GetStringParam('\StringFileInfo\' + LanguageInfo + '\FileVersion');
          FInternalName := GetStringParam('\StringFileInfo\' + LanguageInfo + '\InternalName');
          FLegalCopyright := GetStringParam('\StringFileInfo\' + LanguageInfo + '\LegalCopyright');
          FOriginalFilename := GetStringParam('\StringFileInfo\' + LanguageInfo + '\OriginalFilename');
          FProductName := GetStringParam('\StringFileInfo\' + LanguageInfo + '\ProductName');
          FProductVersion := GetStringParam('\StringFileInfo\' + LanguageInfo + '\ProductVersion');
          FComments := GetStringParam('\StringFileInfo\' + LanguageInfo + '\Comments');
      end;
    finally
      FreeMem(ABuf, ASize);
    end;
  end;
end;

{ TcxSchedulerICalendarCustomParameter }

class function TcxSchedulerICalendarCustomParameter.ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
  var AValue: Variant): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarCustomParameter.ConvertPropertyFromParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
  const AParameterValue: string; var AValue: Variant): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarCustomParameter.GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string;
begin
  Result := '';
end;

class function TcxSchedulerICalendarCustomParameter.TokenName: string;
begin
  Result := '';
end;

class function TcxSchedulerICalendarCustomParameter.FromString(AProperty: TcxSchedulerICalendarCustomProperty;
  const ATranslateValue: string; out AValue: Variant): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarCustomParameter.ToString(AProperty: TcxSchedulerICalendarCustomProperty;
  AValue: Variant; out ATranslateValue: string): Boolean;
begin
  Result := False;
end;

{ TcxSchedulerICalendarCustomProperty }

constructor TcxSchedulerICalendarCustomProperty.Create(AICalendar: TcxSchedulerICalendar; AParent: TcxSchedulerICalendarCustomObject);
begin
  inherited Create;
  FICalendar := AICalendar;
  FCustomParameters := TList.Create;
  RegisterCustomParameters;
  FParent := AParent;
end;

destructor TcxSchedulerICalendarCustomProperty.Destroy;
begin
  FreeAndNil(FCustomParameters);
  inherited;
end;

procedure TcxSchedulerICalendarCustomProperty.Export(AStrings: TStringList);
begin
  if CanExport then
  begin
    BeforeExport(AStrings);
    DoExport(AStrings);
    AfterExport(AStrings);
  end;
end;

procedure TcxSchedulerICalendarCustomProperty.Import(AStrings: TStringList);
begin
  if CanImport then
  begin
    BeforeImport(AStrings);
    DoImport(AStrings);
    AfterImport(AStrings);
  end;
end;

class procedure TcxSchedulerICalendarCustomProperty.Register;
begin
  cxSchedulerICalendarPropertyManager.Add(Self);
end;

class procedure TcxSchedulerICalendarCustomProperty.Unregister;
begin
  cxSchedulerICalendarPropertyManager.Remove(Self);
end;

class function TcxSchedulerICalendarCustomProperty.SchedulerObjectClass: TClass;
begin
  Result := nil;
end;

class function TcxSchedulerICalendarCustomProperty.IsBegin(const S: string): Boolean;
begin
  Result := StrLComp(PChar(S), PChar(TokenName), Length(TokenName)) = 0;
end;

class function TcxSchedulerICalendarCustomProperty.IsEnd(const S: string): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarCustomProperty.TokenName: string;
begin
  Result := '';
end;

procedure TcxSchedulerICalendarCustomProperty.AfterExport(AStrings: TStringList);
begin
// do nothing
end;

procedure TcxSchedulerICalendarCustomProperty.AfterImport(AStrings: TStringList);
begin
  FSchedulerObject := nil;
end;

procedure TcxSchedulerICalendarCustomProperty.BeforeExport(AStrings: TStringList);
begin
// do nothing
end;

procedure TcxSchedulerICalendarCustomProperty.BeforeImport(AStrings: TStringList);
begin
  CreateSchedulerObject;
end;

function TcxSchedulerICalendarCustomProperty.CanExport: Boolean;
begin
  Result := not VarIsNull(Value) and (Length(VarToStr(Value)) > 0);
end;

function TcxSchedulerICalendarCustomProperty.CanImport: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerICalendarCustomProperty.DoExport(AStrings: TStringList);
begin
  AStrings.Add(ToString);
end;

procedure TcxSchedulerICalendarCustomProperty.DoImport(AStrings: TStringList);

  procedure Parse(const S: string; var AValue, AParameters: string);
  var
    I: Integer;
    AIsQuoteOpen: Boolean;
    AIsValueParse: Boolean;
  begin
    AValue := '';
    AParameters := '';
    AIsValueParse := False;
    AIsQuoteOpen := False;
    for I := Length(TokenName) to Length(S) do
    begin
      if S[I] = cxSchedulerICalendarValueDelimiter then
      begin
        if not AIsValueParse and not AIsQuoteOpen then
        begin
          AIsValueParse := True;
          Continue;
        end;
      end;
      if not AIsValueParse and (S[I] = cxSchedulerICalendarQuote) then
        AIsQuoteOpen := not AIsQuoteOpen;
      if AIsValueParse then
        AValue := AValue + S[I]
      else
        AParameters := AParameters + S[I];
    end;
  end;

var
  AParameters: string;
  AValue: string;
begin
  Parse(AStrings[0], AValue, AParameters);
  AStrings.Delete(0);

  SetValue(FromString(AValue, AParameters));
end;

function TcxSchedulerICalendarCustomProperty.FromString(const AValue, AParameters: string): Variant;

  procedure ConvertValue(const S: string; var AValue: Variant);
  var
    I: Integer;
    AParameterName: string;
    AParameterValue: string;
  begin
    if ParameterSplit(S, AParameterName, AParameterValue) then
      for I := 0 to CustomParameterCount - 1 do
        if CompareText(AParameterName, CustomParameters[I].TokenName) = 0 then
          CustomParameters[I].ConvertPropertyFromParameterValue(Self, AParameterValue, AValue);
  end;

var
  I: Integer;
  AStrings: TStrings;
begin
  Result := AValue;
  for I := CustomParameterCount - 1 downto 0 do
    if CustomParameters[I].FromString(Self, AValue, Result) then
      Break;

  AStrings := StrSplit(cxSchedulerICalendarParameterDelimiter, AParameters);
  try
    for I := AStrings.Count - 1 downto 0 do
      ConvertValue(AStrings[I], Result);
  finally
    AStrings.Free;
  end;
end;

function TcxSchedulerICalendarCustomProperty.ToString: string;
var
  AValue: Variant;
  I: Integer;
  ATranslateValue: string;
  AParameters: string;
begin
  AParameters := '';
  AValue := Value;
  ATranslateValue := VarToStr(AValue);
  for I := 0 to CustomParameterCount - 1 do
    if CustomParameters[I].ConvertPropertyToParameterValue(Self, AValue) then
      AParameters := AParameters + Format(cxSchedulerICalendarPropertyParameterFormat,
        [CustomParameters[I].TokenName, CustomParameters[I].GetValue(Self)]);
  for I := 0 to CustomParameterCount - 1 do
    if CustomParameters[I].ToString(Self, AValue, ATranslateValue) then
      Break;
  Result := Format(cxSchedulerICalendarPropertyFormat, [TokenName + AParameters, ATranslateValue]);
end;

procedure TcxSchedulerICalendarCustomProperty.Calculate;
begin
// do nothing
end;

procedure TcxSchedulerICalendarCustomProperty.CreateSchedulerObject;
begin
  if SchedulerObjectClass <> nil then
    DoCreateSchedulerObject;
end;

procedure TcxSchedulerICalendarCustomProperty.DoCreateSchedulerObject;
begin
  FSchedulerObject := SchedulerObjectClass.Create;
end;

procedure TcxSchedulerICalendarCustomProperty.AddCustomParameter(AClass: TcxSchedulerICalendarCustomParameterClass);
begin
  FCustomParameters.Add(AClass);
end;

procedure TcxSchedulerICalendarCustomProperty.RegisterCustomParameters;
begin
// do nothing
end;

function TcxSchedulerICalendarCustomProperty.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TcxSchedulerICalendarCustomProperty.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

function TcxSchedulerICalendarCustomProperty.GetCustomParameterCount: Integer;
begin
  Result := FCustomParameters.Count;
end;

function TcxSchedulerICalendarCustomProperty.GetCustomParameter(Index: Integer): TcxSchedulerICalendarCustomParameterClass;
begin
  Result := TcxSchedulerICalendarCustomParameterClass(FCustomParameters[Index]);
end;

{ TcxSchedulerICalendarCustomObject }

constructor TcxSchedulerICalendarCustomObject.Create(AICalendar: TcxSchedulerICalendar;
  AParent: TcxSchedulerICalendarCustomObject);
begin
  inherited;
  FProperties := TList.Create;
  PopulateProperties;
end;

destructor TcxSchedulerICalendarCustomObject.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

class function TcxSchedulerICalendarCustomObject.IsBegin(const S: string): Boolean;
begin
  Result := CompareText(S, Format(cxSchedulerICalendarObjectBeginFormat, [TokenName])) = 0;
end;

class function TcxSchedulerICalendarCustomObject.IsEnd(const S: string): Boolean;
begin
  Result := CompareText(S, Format(cxSchedulerICalendarObjectEndFormat, [TokenName])) = 0;
end;

procedure TcxSchedulerICalendarCustomObject.AfterExport(AStrings: TStringList);
begin
  inherited;
  AStrings.Add(Format(cxSchedulerICalendarObjectEndFormat, [TokenName]));
  FProperties.Clear;
end;

procedure TcxSchedulerICalendarCustomObject.BeforeExport(AStrings: TStringList);
begin
  inherited;
  AStrings.Add(Format(cxSchedulerICalendarObjectBeginFormat, [TokenName]));
end;

function TcxSchedulerICalendarCustomObject.CanExport: Boolean;
begin
  Result := ICalendar.CanExport(Self);
end;

function TcxSchedulerICalendarCustomObject.CanImport: Boolean;
begin
  Result := ICalendar.CanImport(Self);
end;

procedure TcxSchedulerICalendarCustomObject.DoExport(AStrings: TStringList);
var
  I: Integer;
  AProperty: TcxSchedulerICalendarCustomProperty;
begin
  for I := 0 to PropertyCount - 1 do
  begin
    AProperty := Properties[I].Create(ICalendar, Self);
    try
      AProperty.Calculate;
      AProperty.Export(AStrings);
    finally
      AProperty.Free;
    end;
  end;
end;

procedure TcxSchedulerICalendarCustomObject.DoImport(AStrings: TStringList);

  function GetObjectStrings(AObjectStrings: TStringList): Boolean;
  begin
    Result := IsBegin(AStrings[0]);
    if Result then
    begin
      AStrings.Delete(0);
      while (AStrings.Count > 0) and not IsEnd(AStrings[0]) do
      begin
        AObjectStrings.Add(AStrings[0]);
        AStrings.Delete(0);
      end;
      Result := AStrings.Count > 0;
      if Result then
        AStrings.Delete(0);
    end;
  end;

var
  AObjectStrings: TStringList;
begin
  AObjectStrings := TStringList.Create;
  try
    if GetObjectStrings(AObjectStrings) and (AObjectStrings.Count > 0) then
      while AObjectStrings.Count > 0 do
        ICalendar.ImportObject(Self, AObjectStrings);
  finally
    AObjectStrings.Free;
  end;
end;

procedure TcxSchedulerICalendarCustomObject.AddProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass);
begin
  FProperties.Add(AProperty);
end;

function TcxSchedulerICalendarCustomObject.CanHaveProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass): Boolean;
begin
  Result := FProperties.IndexOf(AProperty) <> -1;
end;

procedure TcxSchedulerICalendarCustomObject.PopulateProperties;
begin
// do nothing
end;

function TcxSchedulerICalendarCustomObject.GetProperty(Index: Integer): TcxSchedulerICalendarCustomPropertyClass;
begin
  Result := TcxSchedulerICalendarCustomPropertyClass(FProperties[Index]);
end;

function TcxSchedulerICalendarCustomObject.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

{ TcxSchedulerICalendarTZIDParameter }

class function TcxSchedulerICalendarTZIDParameter.ConvertPropertyToParameterValue(
  AProperty: TcxSchedulerICalendarCustomProperty; var AValue: Variant): Boolean;
var
  ADateTimeProperty: TcxSchedulerICalendarDateTimeCustomProperty;
begin
  ADateTimeProperty := TcxSchedulerICalendarDateTimeCustomProperty(AProperty);
  Result := ADateTimeProperty.TimeZone <> ADateTimeProperty.ICalendar.TimeZone;
  if Result then
    AValue := ConvertDateToAnotherTimeZone(AValue, ADateTimeProperty.ICalendar.TimeZone,
      ADateTimeProperty.TimeZone);
end;

class function TcxSchedulerICalendarTZIDParameter.ConvertPropertyFromParameterValue(
  AProperty: TcxSchedulerICalendarCustomProperty; const AParameterValue: string; var AValue: Variant): Boolean;
var
  ADateTimeProperty: TcxSchedulerICalendarDateTimeCustomProperty;
  S: string;
begin
  S := Trim(AParameterValue);
  if S[1] = cxSchedulerICalendarQuote then
    Delete(S, 1, 1);
  if S[Length(S)] = cxSchedulerICalendarQuote then
    Delete(S, Length(S), 1);
  ADateTimeProperty := TcxSchedulerICalendarDateTimeCustomProperty(AProperty);
  ADateTimeProperty.TimeZone := ADateTimeProperty.ICalendar.FindTimeZone(S);
  Result := ADateTimeProperty.TimeZone <> ADateTimeProperty.ICalendar.TimeZone;
  if Result then
    AValue := ConvertDateToAnotherTimeZone(AValue,
      ADateTimeProperty.TimeZone, ADateTimeProperty.ICalendar.TimeZone);
end;

class function TcxSchedulerICalendarTZIDParameter.GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string;
var
  ADateTimeProperty: TcxSchedulerICalendarDateTimeCustomProperty;
begin
  ADateTimeProperty := TcxSchedulerICalendarDateTimeCustomProperty(AProperty);
  Result := cxSchedulerICalendarQuote +
    TcxSchedulerDateTimeHelper.TimeZoneInfo(ADateTimeProperty.TimeZone).StandardName + cxSchedulerICalendarQuote;
end;

class function TcxSchedulerICalendarTZIDParameter.TokenName: string;
begin
  Result := 'TZID';
end;

{ TcxSchedulerICalendarLanguageParameter }

class function TcxSchedulerICalendarLanguageParameter.ConvertPropertyToParameterValue(
  AProperty: TcxSchedulerICalendarCustomProperty; var AValue: Variant): Boolean;
begin
  Result := True;
  AValue := dxStringToUTF8String(AValue);
end;

class function TcxSchedulerICalendarLanguageParameter.ConvertPropertyFromParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
  const AParameterValue: string; var AValue: Variant): Boolean;
var
  S: UTF8String;
begin
  Result := True;
  S := UTF8String(AValue);
  AValue := dxUTF8StringToString(S);
end;

class function TcxSchedulerICalendarLanguageParameter.GetValue(
  AProperty: TcxSchedulerICalendarCustomProperty): string;
begin
  with dxLanguages do
    Result := LocaleName[IndexOf(GetDefaultLanguageLCID)];
end;

class function TcxSchedulerICalendarLanguageParameter.TokenName: string;
begin
  Result := 'LANGUAGE';
end;

{ TcxSchedulerICalendarStringParameter }

class function TcxSchedulerICalendarStringParameter.FromString(AProperty: TcxSchedulerICalendarCustomProperty;
  const ATranslateValue: string; out AValue: Variant): Boolean;
begin
  Result := True;
  AValue := StringReplace(ATranslateValue, '\,', ',', [rfReplaceAll]);
  AValue := StringReplace(VarToStr(AValue), '\n', #13#10, [rfReplaceAll]);
end;

class function TcxSchedulerICalendarStringParameter.ToString(AProperty: TcxSchedulerICalendarCustomProperty;
  AValue: Variant; out ATranslateValue: string): Boolean;
begin
  Result := True;
  ATranslateValue := StringReplace(VarToStr(AValue), #13#10, '\n', [rfReplaceAll]);
  ATranslateValue := StringReplace(ATranslateValue, ',', '\,', [rfReplaceAll]);
end;

{ TcxSchedulerICalendarStringCustomProperty }

procedure TcxSchedulerICalendarStringCustomProperty.RegisterCustomParameters;
begin
  inherited;
  AddCustomParameter(TcxSchedulerICalendarLanguageParameter);
  AddCustomParameter(TcxSchedulerICalendarStringParameter);
end;

{ TcxSchedulerICalendarDateParameter }

class function TcxSchedulerICalendarDateParameter.ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty;
  var AValue: Variant): Boolean;
begin
  Result := TcxSchedulerICalendarDateTimeCustomProperty(AProperty).UseDateFormat;
  if Result then
    AValue := dxDateOf(AValue);
end;

class function TcxSchedulerICalendarDateParameter.GetValue(AProperty: TcxSchedulerICalendarCustomProperty): string;
begin
  Result := 'DATE';
end;

class function TcxSchedulerICalendarDateParameter.TokenName: string;
begin
  Result := 'VALUE';
end;

class function TcxSchedulerICalendarDateParameter.FromString(AProperty: TcxSchedulerICalendarCustomProperty;
  const ATranslateValue: string; out AValue: Variant): Boolean;
var
  AResult: TDateTime;
  AYear, AMonth, ADay, AHour, AMinute, ASecond: Word;
  AUseDateFormat: Boolean;
begin
  AYear := StrToInt(Copy(ATranslateValue, 1, 4));
  AMonth := StrToInt(Copy(ATranslateValue, 5, 2));
  ADay := StrToInt(Copy(ATranslateValue, 7, 2));

  AUseDateFormat := Length(ATranslateValue) <= 9;
  if AUseDateFormat then
  begin
    AHour := 0;
    AMinute := 0;
    ASecond := 0;
  end
  else
  begin
    AHour := StrToInt(Copy(ATranslateValue, 10, 2));
    AMinute := StrToInt(Copy(ATranslateValue, 12, 2));
    ASecond := StrToInt(Copy(ATranslateValue, 14, 2));
  end;
  if ATranslateValue[Length(ATranslateValue)] = cxSchedulerICalendarUtcDateTimePostfix then
    TcxSchedulerICalendarDateTimeCustomProperty(AProperty).TimeZone := -1
  else
    TcxSchedulerICalendarDateTimeCustomProperty(AProperty).TimeZone := AProperty.ICalendar.TimeZone;

  Result := TryEncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, 0, AResult);
  if Result then
  begin
    if TcxSchedulerICalendarDateTimeCustomProperty(AProperty).UseTimeZone then
    begin
      if TcxSchedulerICalendarDateTimeCustomProperty(AProperty).TimeZone = -1 then
        AResult := ConvertDateToLocalTime(AResult, AProperty.ICalendar.GetStorageTimeZone)
      else
        if AProperty.ICalendar.TimeZone <> AProperty.ICalendar.GetStorageTimeZone then
        begin
          AResult := ConvertDateToAnotherTimeZone(AResult,
            AProperty.ICalendar.TimeZone, AProperty.ICalendar.GetStorageTimeZone);
        end;
    end;
    AUseDateFormat := AUseDateFormat and (Trunc(AResult) = AResult);
    AValue := AResult;
  end;
  TcxSchedulerICalendarDateTimeCustomProperty(AProperty).UseDateFormat := AUseDateFormat;
end;

class function TcxSchedulerICalendarDateParameter.ToString(AProperty: TcxSchedulerICalendarCustomProperty;
  AValue: Variant; out ATranslateValue: string): Boolean;
begin
  Result := TcxSchedulerICalendarDateTimeCustomProperty(AProperty).UseDateFormat;
  if Result then
    DateTimeToString(ATranslateValue, cxSchedulerICalendarDateFormat, AValue);
end;

{ TcxSchedulerICalendarDateTimeParameter }

class function TcxSchedulerICalendarDateTimeParameter.ConvertPropertyToParameterValue(AProperty: TcxSchedulerICalendarCustomProperty; var AValue: Variant): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarDateTimeParameter.ToString(AProperty: TcxSchedulerICalendarCustomProperty;
  AValue: Variant; out ATranslateValue: string): Boolean;
const
  FormatMap: array[Boolean] of string = (cxSchedulerICalendarIso8601DateTimeFormat, cxSchedulerICalendarIso8601UtcDateTimeFormat);
var
  AUseUtcFormat: Boolean;
begin
  Result := True;
  AUseUtcFormat := TcxSchedulerICalendarDateTimeCustomProperty(AProperty).TimeZone = -1;
  if TcxSchedulerICalendarDateTimeCustomProperty(AProperty).UseTimeZone and AUseUtcFormat then
    AValue := ConvertDateToGlobalTime(AValue, AProperty.ICalendar.GetStorageTimeZone);
  DateTimeToString(ATranslateValue, FormatMap[AUseUtcFormat], AValue);
end;

{ TcxSchedulerICalendarDateTimeCustomProperty }

procedure TcxSchedulerICalendarDateTimeCustomProperty.RegisterCustomParameters;
begin
  AddCustomParameter(TcxSchedulerICalendarTZIDParameter);
  AddCustomParameter(TcxSchedulerICalendarDateParameter);
  AddCustomParameter(TcxSchedulerICalendarDateTimeParameter);
end;

function TcxSchedulerICalendarDateTimeCustomProperty.UseTimeZone: Boolean;
begin
  Result := True;
end;

{ TcxSchedulerICalendarRecurrenceUntilProperty }

function TcxSchedulerICalendarRecurrenceUntilProperty.UseTimeZone: Boolean;
begin
  Result := False;
end;

{ TcxSchedulerICalendarTimeZoneStartProperty }

procedure TcxSchedulerICalendarTimeZoneStartProperty.Calculate;
begin
  inherited;
  TimeZone := ICalendar.TimeZone;
end;

class function TcxSchedulerICalendarTimeZoneStartProperty.TokenName: string;
begin
  Result := 'DTSTART';
end;

function TcxSchedulerICalendarTimeZoneStartProperty.GetValue: Variant;
begin
  Result := Parent.GetStart;
end;

procedure TcxSchedulerICalendarTimeZoneStartProperty.SetValue(const AValue: Variant);
var
  ADate: TSystemTime;
begin
  inherited SetValue(AValue);
  if ICalendar.TimeZone = -1 then
  begin
    DateTimeToSystemTime(AValue, ADate);
    if Parent.IsStandardRule then
      Parent.Parent.FInternalTimeZone.TZI.StandardDate := ADate
    else
      Parent.Parent.FInternalTimeZone.TZI.DaylightDate := ADate;
  end;
end;

function TcxSchedulerICalendarTimeZoneStartProperty.GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
begin
  Result := TcxSchedulerICalendarTimeZoneCustomRule(inherited Parent);
end;

{ TcxSchedulerICalendarTZIDProperty }

class function TcxSchedulerICalendarTZIDProperty.TokenName: string;
begin
  Result := 'TZID';
end;

procedure TcxSchedulerICalendarTZIDProperty.RegisterCustomParameters;
begin
  inherited;
  AddCustomParameter(TcxSchedulerICalendarStringParameter);
end;

function TcxSchedulerICalendarTZIDProperty.GetValue: Variant;
begin
  Result := Parent.GetTimeZoneInformation.StandardName;
end;

procedure TcxSchedulerICalendarTZIDProperty.SetValue(const AValue: Variant);
begin
  ICalendar.TimeZone := ICalendar.FindTimeZone(AValue);
  if ICalendar.TimeZone = -1 then
    Parent.FInternalTimeZone.Display := AValue;
end;

function TcxSchedulerICalendarTZIDProperty.GetParent: TcxSchedulerICalendarTimeZoneObject;
begin
  Result := TcxSchedulerICalendarTimeZoneObject(inherited Parent);
end;

{ TcxSchedulerICalendarTimeZoneRuleCustomProperty }

function TcxSchedulerICalendarTimeZoneRuleCustomProperty.GetCurrentTimeZone: Integer;
begin
  Result := Parent.Parent.GetCurrentTimeZone;
end;

function TcxSchedulerICalendarTimeZoneRuleCustomProperty.GetTimeZoneInformation: TcxTimeZoneInformation;
begin
  Result := Parent.Parent.GetTimeZoneInformation;
end;

function TcxSchedulerICalendarTimeZoneRuleCustomProperty.FromUtcOffset(const AValue: TDateTime): string;
const
  TimeFormat = 'hhmm';
begin
  if AValue > 0 then
    Result := '+'
  else
    Result := '-';

  Result := Result + cxTimeToStr(AValue, TimeFormat);
end;

function TcxSchedulerICalendarTimeZoneRuleCustomProperty.GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
begin
  Result := TcxSchedulerICalendarTimeZoneCustomRule(inherited Parent);
end;

{ TcxSchedulerICalendarTZOffsetToProperty }

class function TcxSchedulerICalendarTZOffsetToProperty.TokenName: string;
begin
  Result := 'TZOFFSETTO';
end;

function TcxSchedulerICalendarTZOffsetToProperty.GetBiasFromStr(const AValue: string): Integer;

  function GetHourValue(const ADateString: string): Integer;
  begin
    Result := StrToInt(Copy(ADateString, 1, 2));
  end;

  function GetMinuteValue(const ADateString: string): Integer;
  begin
    Result := StrToInt(Copy(ADateString, 3, 2));
  end;

var
  ADateString: string;
  AIsMinus: Boolean;
begin
  ADateString := AValue;
  AIsMinus := ADateString[1] = '-';
  Delete(ADateString, 1, 1);
  Result := GetHourValue(ADateString) * MinsPerHour + GetMinuteValue(ADateString);
  if AIsMinus then
    Result := - Result;
end;

function TcxSchedulerICalendarTZOffsetToProperty.GetValue: Variant;
begin
  if Parent.IsStandardRule then
    Result := FromUtcOffset(Parent.FUtcOffset)
  else
    Result := FromUtcOffset(Parent.FUtcOffset - Parent.FBias);
end;

procedure TcxSchedulerICalendarTZOffsetToProperty.SetValue(const AValue: Variant);
begin
  inherited SetValue(AValue);
  if ICalendar.TimeZone <> -1 then
    Exit;
  if Parent.IsStandardRule then
    Parent.Parent.FInternalTimeZone.TZI.Bias := - GetBiasFromStr(AValue)
  else
    Parent.Parent.FInternalTimeZone.TZI.DaylightBias := - GetBiasFromStr(AValue);
end;

{ TcxSchedulerICalendarTZOffsetFromProperty }

class function TcxSchedulerICalendarTZOffsetFromProperty.TokenName: string;
begin
  Result := 'TZOFFSETFROM';
end;

function TcxSchedulerICalendarTZOffsetFromProperty.GetValue: Variant;
begin
  if Parent.IsStandardRule then
    Result := FromUtcOffset(Parent.FUtcOffset - Parent.FBias)
  else
    Result := FromUtcOffset(Parent.FUtcOffset);
end;

{ TcxSchedulerICalendarTimeZoneRuleRecurrence }

function TcxSchedulerICalendarTimeZoneRuleRecurrence.CanExport: Boolean;
begin
  Result := inherited CanExport and not Parent.Parent.UseOnlyStandardTime;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetByDay: TDays;
begin
  Result := [Parent.GetDayOfWeek];
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetByDayWeek(ADay: TDay): Integer;
begin
  if Parent.Parent.UseOnlyStandardTime then
    Result := cxSchedulerICalendarBySetPosInvalidValue
  else
  begin
    Result := DayOf(Parent.GetStart);
    if Result = 5 then
      Result := -1;
  end;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetByMonth: Integer;
begin
  Result := MonthOf(Parent.GetStart);
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetByMonthDay: Integer;
begin
  Result := 0;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetBySetPos: Integer;
begin
  Result := cxSchedulerICalendarBySetPosInvalidValue;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetCount: Integer;
begin
  Result := -1;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetInterval: Integer;
begin
  Result := 1;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetRecurrenceType: TcxRecurrence;
begin
  Result := cxreYearly;
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetRecurrenceUntil: TDateTime;
begin
  Result := 0;
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetByDay(AValue: TDays);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetByDayWeek(ADay: TDay; AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetByMonth(AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetByMonthDay(AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetBySetPos(AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetCount(AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetInterval(AValue: Integer);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetRecurrenceType(AValue: TcxRecurrence);
begin
// do nothing
end;

procedure TcxSchedulerICalendarTimeZoneRuleRecurrence.SetRecurrenceUntil(AValue: TDateTime);
begin
// do nothing
end;

function TcxSchedulerICalendarTimeZoneRuleRecurrence.GetParent: TcxSchedulerICalendarTimeZoneCustomRule;
begin
  Result := TcxSchedulerICalendarTimeZoneCustomRule(inherited Parent);
end;

{ TcxSchedulerICalendarTimeZoneCustomRule }

procedure TcxSchedulerICalendarTimeZoneCustomRule.Calculate;
begin
  inherited;
  with Parent.GetTimeZoneInformation.TZI do
    FBias := (DaylightBias - StandardBias) / MinsPerDay;
  FUtcOffset := - Parent.GetTimeZoneInformation.TZI.Bias / MinsPerDay;
end;

function TcxSchedulerICalendarTimeZoneCustomRule.CanExport: Boolean;
begin
  Result := inherited CanExport and (IsStandardRule or not Parent.UseOnlyStandardTime);
end;

procedure TcxSchedulerICalendarTimeZoneCustomRule.PopulateProperties;
begin
  inherited;
  AddProperty(TcxSchedulerICalendarTimeZoneStartProperty);
  AddProperty(TcxSchedulerICalendarTimeZoneRuleRecurrence);
  AddProperty(TcxSchedulerICalendarTZOffsetFromProperty);
  AddProperty(TcxSchedulerICalendarTZOffsetToProperty);
end;

function TcxSchedulerICalendarTimeZoneCustomRule.GetDayOfWeek: TDay;
begin
  if IsStandardRule then
    Result := Parent.GetStandardDayOfWeek
  else
    Result := Parent.GetDaylightDayOfWeek;
end;

function TcxSchedulerICalendarTimeZoneCustomRule.IsStandardRule: Boolean;
begin
  Result := True;
end;

function TcxSchedulerICalendarTimeZoneCustomRule.GetStart: TDateTime;
begin
  if IsStandardRule then
    Result := Parent.GetStandartDate
  else
    Result := Parent.GetDaylightDate;
end;

function TcxSchedulerICalendarTimeZoneCustomRule.GetParent: TcxSchedulerICalendarTimeZoneObject;
begin
  Result := TcxSchedulerICalendarTimeZoneObject(inherited Parent);
end;

{ TcxSchedulerICalendarTimeZoneObject }

class function TcxSchedulerICalendarTimeZoneObject.TokenName: string;
begin
  Result := 'VTIMEZONE';
end;

procedure TcxSchedulerICalendarTimeZoneObject.PopulateProperties;
begin
  inherited;
  AddProperty(TcxSchedulerICalendarTZIDProperty);
  AddProperty(TcxSchedulerICalendarTimeZoneStandardRule);
  AddProperty(TcxSchedulerICalendarTimeZoneDaylightRule);
end;

function TcxSchedulerICalendarTimeZoneObject.GetDaylightDate: TDateTime;
begin
  Result := InternalSystemTimeToDateTime(GetTimeZoneInformation.TZI.DaylightDate);
end;

function TcxSchedulerICalendarTimeZoneObject.GetStandartDate: TDateTime;
begin
  Result := InternalSystemTimeToDateTime(GetTimeZoneInformation.TZI.StandardDate);
end;

function TcxSchedulerICalendarTimeZoneObject.GetDaylightDayOfWeek: TDay;
begin
  Result := TDay(GetTimeZoneInformation.TZI.DaylightDate.wDayOfWeek);
end;

function TcxSchedulerICalendarTimeZoneObject.GetStandardDayOfWeek: TDay;
begin
  Result := TDay(GetTimeZoneInformation.TZI.StandardDate.wDayOfWeek);
end;

procedure TcxSchedulerICalendarTimeZoneObject.AfterImport(
  AStrings: TStringList);
begin
  inherited;
  if ICalendar.TimeZone = -1 then
  begin
    if (FInternalTimeZone.TZI.DaylightBias <> 0) and (FInternalTimeZone.TZI.Bias <> 0) then
      FInternalTimeZone.TZI.DaylightBias := FInternalTimeZone.TZI.DaylightBias - FInternalTimeZone.TZI.Bias;
    ICalendar.TimeZone := TcxSchedulerICalendarDateTimeHelper.AddInternalTimeZone(FInternalTimeZone);
  end;
end;

function TcxSchedulerICalendarTimeZoneObject.GetCurrentTimeZone: Integer;
begin
  Result := ICalendar.TimeZone;
end;

function TcxSchedulerICalendarTimeZoneObject.GetTimeZoneInformation: TcxTimeZoneInformation;
begin
  Result := TcxSchedulerDateTimeHelper.TimeZoneInfo(GetCurrentTimeZone);
end;

function TcxSchedulerICalendarTimeZoneObject.UseOnlyStandardTime: Boolean;
begin
  Result := GetStandartDate = GetDaylightDate;
end;

function TcxSchedulerICalendarTimeZoneObject.InternalSystemTimeToDateTime(ADate: TSystemTime): TDateTime;
begin
  ADate.wYear := Max(ADate.wYear, 1601);
  ADate.wMonth := Max(ADate.wMonth, 1);
  ADate.wDay := Max(ADate.wDay, 1);
  Result := SystemTimeToDateTime(ADate);
end;

{ TcxSchedulerICalendarEventDateTimeProperty }

procedure TcxSchedulerICalendarEventDateTimeProperty.Calculate;
begin
  inherited;
  TimeZone := ICalendar.TimeZone;
  UseDateFormat := Event.AllDayEvent;
end;

function TcxSchedulerICalendarEventDateTimeProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := Parent.Event;
end;

function TcxSchedulerICalendarEventDateTimeProperty.GetParent: TcxSchedulerICalendarEventObject;
begin
  Result := TcxSchedulerICalendarEventObject(inherited Parent);
end;

{ TcxSchedulerICalendarEventStampProperty }

class function TcxSchedulerICalendarEventStampProperty.TokenName: string;
begin
  Result := 'DTSTAMP';
end;

function TcxSchedulerICalendarEventStampProperty.GetValue: Variant;
begin
  Result := ICalendar.FDateTime;
end;

{ TcxSchedulerICalendarEventStartProperty }

class function TcxSchedulerICalendarEventStartProperty.TokenName: string;
begin
  Result := 'DTSTART';
end;

function TcxSchedulerICalendarEventStartProperty.GetValue: Variant;
begin
  Result := Event.Start;
end;

procedure TcxSchedulerICalendarEventStartProperty.SetValue(const AValue: Variant);
begin
  Event.AllDayEvent := UseDateFormat;
  Event.Start := AValue;
end;

{ TcxSchedulerICalendarEventEndProperty }

class function TcxSchedulerICalendarEventEndProperty.TokenName: string;
begin
  Result := 'DTEND';
end;

function TcxSchedulerICalendarEventEndProperty.GetValue: Variant;
begin
  Result := Event.Finish;
end;

procedure TcxSchedulerICalendarEventEndProperty.SetValue(const AValue: Variant);
begin
  Event.AllDayEvent := UseDateFormat;
  Event.Finish := AValue;
end;

{ TcxSchedulerICalendarEventCustomProperty }

function TcxSchedulerICalendarEventCustomProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := Parent.Event;
end;

function TcxSchedulerICalendarEventCustomProperty.GetParent: TcxSchedulerICalendarEventObject;
begin
  Result := TcxSchedulerICalendarEventObject(inherited Parent);
end;

{ TcxSchedulerICalendarEventUIDProperty }

class function TcxSchedulerICalendarEventUIDProperty.TokenName: string;
begin
  Result := 'UID';
end;

function TcxSchedulerICalendarEventUIDProperty.GetValue: Variant;
begin
  Result := dxGenerateGUID;
end;

{ TcxSchedulerICalendarRecurrenceCustomValue }

class function TcxSchedulerICalendarRecurrenceCustomValue.TokenName: string;
begin
  Result := '';
end;

class function TcxSchedulerICalendarRecurrenceCustomValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
begin
  Result := False;
end;

class function TcxSchedulerICalendarRecurrenceCustomValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := False;
end;

{ TcxSchedulerICalendarRecurrenceTypeValue }

class function TcxSchedulerICalendarRecurrenceTypeValue.TokenName: string;
begin
  Result := 'FREQ';
end;

const
  RecurrenceTypeMap: array[TcxRecurrence] of string = ('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY');

class function TcxSchedulerICalendarRecurrenceTypeValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  AType: TcxRecurrence;
begin
  Result := True;
  for AType := Low(TcxRecurrence) to High(TcxRecurrence) do
    if CompareText(RecurrenceTypeMap[AType], AValue) = 0 then
      Break;
  AProperty.RecurrenceType := AType;
end;

class function TcxSchedulerICalendarRecurrenceTypeValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := True;
  AValue := RecurrenceTypeMap[AProperty.RecurrenceType];
end;

{ TcxSchedulerICalendarRecurrenceCountValue }

class function TcxSchedulerICalendarRecurrenceCountValue.TokenName: string;
begin
  Result := 'COUNT';
end;

class function TcxSchedulerICalendarRecurrenceCountValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  ACount: Integer;
begin
  Result := TryStrToInt(AValue, ACount);
  if Result then
    AProperty.Count := ACount;
end;

class function TcxSchedulerICalendarRecurrenceCountValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := AProperty.Count > 0;
  if Result then
    AValue := IntToStr(AProperty.Count);
end;

{ TcxSchedulerICalendarRecurrenceUntilValue }

class function TcxSchedulerICalendarRecurrenceUntilValue.TokenName: string;
begin
  Result := 'UNTIL';
end;

class function TcxSchedulerICalendarRecurrenceUntilValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  ADateProperty: TcxSchedulerICalendarDateTimeCustomProperty;
  ADate: Variant;
begin
  ADateProperty := TcxSchedulerICalendarRecurrenceUntilProperty.Create(AProperty.ICalendar, nil);
  try
    ADateProperty.TimeZone := -1;
    Result := TcxSchedulerICalendarDateTimeParameter.FromString(ADateProperty, AValue, ADate);
    if Result then
      AProperty.RecurrenceUntil := ADate;
  finally
    ADateProperty.Free;
  end;
end;

class function TcxSchedulerICalendarRecurrenceUntilValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
var
  ADateProperty: TcxSchedulerICalendarDateTimeCustomProperty;
begin
  Result := AProperty.Count = 0;
  if Result then
  begin
    ADateProperty := TcxSchedulerICalendarRecurrenceUntilProperty.Create(AProperty.ICalendar, nil);
    try
      ADateProperty.TimeZone := -1;
      Result := TcxSchedulerICalendarDateTimeParameter.ToString(ADateProperty, AProperty.RecurrenceUntil, AValue);
    finally
      ADateProperty.Free;
    end;
  end;
end;

{ TcxSchedulerICalendarRecurrenceIntervalValue. }

class function TcxSchedulerICalendarRecurrenceIntervalValue.TokenName: string;
begin
  Result := 'INTERVAL';
end;

class function TcxSchedulerICalendarRecurrenceIntervalValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  AInterval: Integer;
begin
  Result := TryStrToInt(AValue, AInterval);
  if Result then
    AProperty.Interval := AInterval;
end;

class function TcxSchedulerICalendarRecurrenceIntervalValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := AProperty.Interval > 1;
  if Result then
    AValue := IntToStr(AProperty.Interval);
end;

{ TcxSchedulerICalendarRecurrenceByDayValue }

class function TcxSchedulerICalendarRecurrenceByDayValue.TokenName: string;
begin
  Result := 'BYDAY';
end;

const
  DaysMap: array[TDay] of string = ('SU', 'MO', 'TU', 'WE', 'TH', 'FR', 'SA');

class function TcxSchedulerICalendarRecurrenceByDayValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;

  function GetDay(const S: string; out ADay: TDay; out AWeekNumber: Integer): Boolean;
  var
    I: Integer;
    AWeek: string;
    ADayName: string;
  begin
    AWeekNumber := cxSchedulerICalendarBySetPosInvalidValue;
    AWeek := '';
    I := 1;
    while (Length(S) >= I) and (Pos(S[I], '-+1234') > 0) do
    begin
      AWeek := AWeek + S[I];
      Inc(I);
    end;
    ADayName := S;
    Delete(ADayName, 1, Length(AWeek));
    ADay := Low(TDay);
    repeat
      Result := CompareText(ADayName, DaysMap[ADay]) = 0;
      if not Result then
        Inc(ADay);
    until Result or (ADay > High(TDay));
    if Result and TryStrToInt(AWeek, I) then
      AWeekNumber := I;
  end;

var
  ADay: TDay;
  ADays: TDays;
  AWeek: Integer;
  AWeeks: array[TDay] of Integer;
  AStrings: TStrings;
  I: Integer;
begin
  ADays := [];
  AStrings := StrSplit(cxSchedulerICalendarDayDelimiter, AValue);
  try
    for I := 0 to AStrings.Count - 1 do
    begin
      if GetDay(AStrings[I], ADay, AWeek) then
      begin
        ADays := ADays + [ADay];
        AWeeks[ADay] := AWeek;
      end;
    end;
  finally
    AStrings.Free;
  end;

  Result := ADays <> [];
  if Result then
  begin
    AProperty.ByDay := ADays;
    for ADay := Low(TDay) to High(TDay) do
      if ADay in ADays then
        AProperty.ByDayWeek[ADay] := AWeeks[ADay];
  end;
end;

class function TcxSchedulerICalendarRecurrenceByDayValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;

  function DaysToString(ADays: TDays): string;
  var
    ADay: TDay;
    AOrdWeek: Integer;
  begin
    Result := '';
    for ADay := Low(TDay) to High(TDay) do
      if ADay in ADays then
      begin
        if Length(Result) > 0 then
          Result := Result + cxSchedulerICalendarDayDelimiter;
        AOrdWeek := AProperty.ByDayWeek[ADay];
        if AOrdWeek <> cxSchedulerICalendarBySetPosInvalidValue then
          Result := Result + IntToStr(AOrdWeek);
        Result := Result + DaysMap[ADay];
      end;
  end;

begin
  Result := AProperty.ByDay <> [];
  if Result then
    AValue := DaysToString(AProperty.ByDay);
end;

{ TcxSchedulerICalendarRecurrenceByMonthValue }

class function TcxSchedulerICalendarRecurrenceByMonthValue.TokenName: string;
begin
  Result := 'BYMONTH';
end;

class function TcxSchedulerICalendarRecurrenceByMonthValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  AMonth: Integer;
begin
  Result := TryStrToInt(AValue, AMonth);
  if Result then
    AProperty.ByMonth := AMonth;
end;

class function TcxSchedulerICalendarRecurrenceByMonthValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := AProperty.ByMonth in [1..12];
  if Result then
    AValue := IntToStr(AProperty.ByMonth);
end;

{ TcxSchedulerICalendarRecurrenceByMonthDayValue }

class function TcxSchedulerICalendarRecurrenceByMonthDayValue.TokenName: string;
begin
  Result := 'BYMONTHDAY';
end;

class function TcxSchedulerICalendarRecurrenceByMonthDayValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  ADay: Integer;
begin
  Result := TryStrToInt(AValue, ADay);
  if Result then
    AProperty.ByMonthDay := ADay;
end;

class function TcxSchedulerICalendarRecurrenceByMonthDayValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := AProperty.ByMonthDay > 0;
  if Result then
    AValue := IntToStr(AProperty.ByMonthDay);
end;

{  TcxSchedulerICalendarRecurrenceBySetPosValue }

class function TcxSchedulerICalendarRecurrenceBySetPosValue.TokenName: string;
begin
  Result := 'BYSETPOS';
end;

class function TcxSchedulerICalendarRecurrenceBySetPosValue.FromString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; const AValue: string): Boolean;
var
  ANumber: Integer;
begin
  Result := TryStrToInt(AValue, ANumber);
  if Result then
    AProperty.BySetPos := ANumber;
end;

class function TcxSchedulerICalendarRecurrenceBySetPosValue.ToString(
  AProperty: TcxSchedulerICalendarRecurrenceCustomProperty; out AValue: string): Boolean;
begin
  Result := (AProperty.BySetPos >= -1) and (AProperty.BySetPos <= 4);
  if Result then
    AValue := IntToStr(AProperty.BySetPos);
end;

{ TcxSchedulerICalendarRecurrenceCustomProperty }

constructor TcxSchedulerICalendarRecurrenceCustomProperty.Create(
  AICalendar: TcxSchedulerICalendar; AParent: TcxSchedulerICalendarCustomObject);
begin
  inherited;
  FValueParameters := TList.Create;
  PopulateValueParameters;
end;

destructor TcxSchedulerICalendarRecurrenceCustomProperty.Destroy;
begin
  FreeAndNil(FValueParameters);
  inherited;
end;

class function TcxSchedulerICalendarRecurrenceCustomProperty.TokenName: string;
begin
  Result := 'RRULE';
end;

function TcxSchedulerICalendarRecurrenceCustomProperty.GetValue: Variant;
var
  S: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to ValueParameterCount - 1 do
    if ValueParameters[I].ToString(Self, S) then
    begin
      if Length(VarToStr(Result)) > 0 then
        Result := Result + cxSchedulerICalendarParameterDelimiter;
      Result := Result + Format(cxSchedulerICalendarPropertyValueParameterFormat,
        [ValueParameters[I].TokenName, S]);
    end;
end;

procedure TcxSchedulerICalendarRecurrenceCustomProperty.SetValue(const AValue: Variant);

  procedure SetValueParameter(const S: string);
  var
    AParameterName: string;
    AValue: string;
    I: Integer;
  begin
    if ParameterSplit(S, AParameterName, AValue) then
      for I := 0 to ValueParameterCount - 1 do
        if CompareText(AParameterName, ValueParameters[I].TokenName) = 0 then
        begin
          ValueParameters[I].FromString(Self, AValue);
          Break;
        end;
  end;

var
  AStrings: TStrings;
  I: Integer;
begin
  AStrings := StrSplit(cxSchedulerICalendarParameterDelimiter, VarToStr(AValue));
  try
    for I := 0 to AStrings.Count - 1 do
      SetValueParameter(AStrings[I]);
  finally
    AStrings.Free;
  end;
end;

procedure TcxSchedulerICalendarRecurrenceCustomProperty.AddValueParameter(
  AParameter: TcxSchedulerICalendarRecurrenceCustomValueClass);
begin
  FValueParameters.Add(AParameter);
end;

procedure TcxSchedulerICalendarRecurrenceCustomProperty.PopulateValueParameters;
begin
  AddValueParameter(TcxSchedulerICalendarRecurrenceTypeValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceIntervalValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceCountValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceUntilValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceByDayValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceByMonthValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceByMonthDayValue);
  AddValueParameter(TcxSchedulerICalendarRecurrenceBySetPosValue);
end;

function TcxSchedulerICalendarRecurrenceCustomProperty.GetByDayWeek(ADay: TDay): Integer;
begin
  Result := cxSchedulerICalendarBySetPosInvalidValue;
end;

procedure TcxSchedulerICalendarRecurrenceCustomProperty.SetByDayWeek(ADay: TDay; AValue: Integer);
begin
// do nothing
end;

function TcxSchedulerICalendarRecurrenceCustomProperty.GetValueParameter(
  Index: Integer): TcxSchedulerICalendarRecurrenceCustomValueClass;
begin
  Result := TcxSchedulerICalendarRecurrenceCustomValueClass(FValueParameters[Index]);
end;

function TcxSchedulerICalendarRecurrenceCustomProperty.GetValueParameterCount: Integer;
begin
  Result := FValueParameters.Count;
end;

{ TcxSchedulerICalendarEventRecurrenceProperty }

function TcxSchedulerICalendarEventRecurrenceProperty.CanExport: Boolean;
begin
  Result := inherited CanExport and Event.IsRecurring;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetValue(const AValue: Variant);
begin
  Event.EventType := etPattern;
  inherited;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetByDay: TDays;
begin
  Result := [];
  if (RecurrenceInfo.Recurrence <> cxreWeekly) and (RecurrenceInfo.DayType = cxdtDay) then
    Exit;
  if RecurrenceInfo.Recurrence = cxreWeekly then
    Result := RecurrenceInfo.OccurDays
  else
    case RecurrenceInfo.DayType of
      cxdtWeekDay: Result := TcxSchedulerDateTimeHelper.WorkDays;
      cxdtWeekEndDay: Result := [dSunday..dSaturday] - TcxSchedulerDateTimeHelper.WorkDays;
    else
      Result := [TDay(Ord(RecurrenceInfo.DayType) - Ord(cxdtSunday))];
    end;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetByMonth: Integer;
begin
  if RecurrenceInfo.Recurrence = cxreYearly then
    Result := RecurrenceInfo.Periodicity
  else
    Result := 0;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetByMonthDay: Integer;
begin
  if (RecurrenceInfo.Recurrence in [cxreMonthly, cxreYearly]) and (RecurrenceInfo.DayNumber > 0) and
      (RecurrenceInfo.DayType = cxdtDay) then
    Result := RecurrenceInfo.DayNumber
  else
    Result := 0;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetBySetPos: Integer;
begin
  if (RecurrenceInfo.Recurrence in [cxreMonthly, cxreYearly]) and
    (RecurrenceInfo.DayType in [cxdtEveryDay..cxdtSaturday]) then
  begin
    Result := RecurrenceInfo.DayNumber;
    if Result = 5 then
      Result := -1;
  end
  else
    Result := cxSchedulerICalendarBySetPosInvalidValue;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetCount: Integer;
begin
  Result := RecurrenceInfo.Count;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetInterval: Integer;
begin
  if RecurrenceInfo.Recurrence = cxreYearly then
    Result := RecurrenceInfo.YearPeriodicity
  else
    Result := RecurrenceInfo.Periodicity;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetRecurrenceType: TcxRecurrence;
begin
  Result := RecurrenceInfo.Recurrence;
  if (Result = cxreDaily) and (RecurrenceInfo.DayType = cxdtWeekDay) then
    Result := cxreWeekly;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetRecurrenceUntil: TDateTime;
begin
  Result := DateOf(RecurrenceInfo.Finish) + 1 - 1 / SecsPerDay;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetByDay(AValue: TDays);
var
  ADay: TDay;
begin
  if (RecurrenceType in [cxreMonthly, cxreYearly]) then
  begin
    for ADay := Low(TDay) to High(TDay) do
      if ADay in AValue then
      begin
        Exclude(AValue, ADay);
        Break;
      end;

    if AValue = [] then
      RecurrenceInfo.DayType := TcxDayType(Ord(ADay) + Ord(cxdtSunday))
    else
    begin
      if AValue + [ADay] = TcxSchedulerDateTimeHelper.WorkDays then
        RecurrenceInfo.DayType := cxdtWeekDay
      else
        RecurrenceInfo.DayType := cxdtWeekEndDay;
    end;
  end
  else
    RecurrenceInfo.OccurDays := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetByDayWeek(ADay: TDay;
  AValue: Integer);
begin
  RecurrenceInfo.DayNumber := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetByMonth(AValue: Integer);
begin
  if RecurrenceInfo.Recurrence = cxreYearly then
    RecurrenceInfo.Periodicity := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetByMonthDay(AValue: Integer);
begin
  RecurrenceInfo.DayType := cxdtDay;
  RecurrenceInfo.DayNumber := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetBySetPos(AValue: Integer);
begin
  if RecurrenceInfo.Recurrence in [cxreMonthly, cxreYearly] then
  begin
    if RecurrenceInfo.DayType = cxdtDay then
      RecurrenceInfo.DayType := cxdtEveryDay;
    if AValue = -1 then
      AValue := 5;
    RecurrenceInfo.DayNumber := AValue;
  end;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetCount(AValue: Integer);
begin
  RecurrenceInfo.Count := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetInterval(AValue: Integer);
begin
  if RecurrenceInfo.Recurrence = cxreDaily then
    RecurrenceInfo.DayType := cxdtEveryDay
  else
    RecurrenceInfo.DayType := cxdtDay;

  if RecurrenceInfo.Recurrence = cxreYearly then
    RecurrenceInfo.YearPeriodicity := AValue
  else
    RecurrenceInfo.Periodicity := AValue;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetRecurrenceType(AValue: TcxRecurrence);
begin
  RecurrenceInfo.Recurrence := AValue;
  if AValue = cxreYearly then
  begin
    RecurrenceInfo.Periodicity := MonthOf(RecurrenceInfo.Event.Start);
    RecurrenceInfo.DayNumber := DayOfTheMonth(RecurrenceInfo.Event.Start);
  end;
end;

procedure TcxSchedulerICalendarEventRecurrenceProperty.SetRecurrenceUntil(AValue: TDateTime);
begin
  RecurrenceInfo.Finish := DateOf(AValue - 1 / SecsPerDay);
  RecurrenceInfo.Count := 0;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := Parent.Event;
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetParent: TcxSchedulerICalendarEventObject;
begin
  Result := TcxSchedulerICalendarEventObject(inherited Parent);
end;

function TcxSchedulerICalendarEventRecurrenceProperty.GetRecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
begin
  Result := Event.RecurrenceInfo;
end;

{ TcxSchedulerICalendarEventStringProperty }

function TcxSchedulerICalendarEventStringProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := Parent.Event;
end;

function TcxSchedulerICalendarEventStringProperty.GetParent: TcxSchedulerICalendarEventObject;
begin
  Result := TcxSchedulerICalendarEventObject(inherited Parent);
end;

{ TcxSchedulerICalendarEventDescriptionProperty }

class function TcxSchedulerICalendarEventDescriptionProperty.TokenName: string;
begin
  Result := 'DESCRIPTION';
end;

function TcxSchedulerICalendarEventDescriptionProperty.GetValue: Variant;
begin
  Result := Event.Message;
end;

procedure TcxSchedulerICalendarEventDescriptionProperty.SetValue(const AValue: Variant);
begin
  Event.Message := AValue;
end;

{ TcxSchedulerICalendarEventLocationProperty }

class function TcxSchedulerICalendarEventLocationProperty.TokenName: string;
begin
  Result := 'LOCATION';
end;

function TcxSchedulerICalendarEventLocationProperty.GetValue: Variant;
begin
  Result := Event.Location;
end;

procedure TcxSchedulerICalendarEventLocationProperty.SetValue(const AValue: Variant);
begin
  Event.Location := AValue;
end;

{ TcxSchedulerICalendarEventSummaryProperty }

class function TcxSchedulerICalendarEventSummaryProperty.TokenName: string;
begin
  Result := 'SUMMARY';
end;

function TcxSchedulerICalendarEventSummaryProperty.GetValue: Variant;
begin
  Result := Event.Caption;
end;

procedure TcxSchedulerICalendarEventSummaryProperty.SetValue(const AValue: Variant);
begin
  Event.Caption := AValue;
end;

{ TcxSchedulerICalendarEventStatusProperty }

class function TcxSchedulerICalendarEventStatusProperty.TokenName: string;
begin
  Result := 'X-MICROSOFT-CDO-BUSYSTATUS';
end;

const
  StatusMap: array[tlsFree..tlsOutOfOffice] of string = ('FREE', 'TENTATIVE', 'BUSY', 'OOF');

function TcxSchedulerICalendarEventStatusProperty.GetValue: Variant;
begin
  Result := StatusMap[Event.State];
end;

procedure TcxSchedulerICalendarEventStatusProperty.SetValue(const AValue: Variant);
var
  AStatus: Integer;
  I: Integer;
begin
  AStatus := tlsFree;
  for I := tlsFree to tlsOutOfOffice do
    if CompareText(AValue, StatusMap[I]) = 0 then
    begin
      AStatus := I;
      Break;
    end;
  Event.State := AStatus;
end;

{ TcxSchedulerICalendarEventLabelProperty }

class function TcxSchedulerICalendarEventLabelProperty.TokenName: string;
begin
  Result := 'X-DEVEXPRESS-LABEL';
end;

function TcxSchedulerICalendarEventLabelProperty.GetValue: Variant;
begin
  Result := Event.LabelColor;
end;

procedure TcxSchedulerICalendarEventLabelProperty.SetValue(const AValue: Variant);
begin
  Event.LabelColor := AValue;
end;

{ TcxSchedulerICalendarEventResourcesProperty }

class function TcxSchedulerICalendarEventResourcesProperty.TokenName: string;
begin
  Result := 'RESOURCES';
end;

function TcxSchedulerICalendarEventResourcesProperty.GetValue: Variant;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Event.ResourceIDCount - 1 do
  begin
    if Length(Result) > 0 then
      Result := Result + cxSchedulerICalendarResourceDelimiter;
    Result := Result + ICalendar.Storage.Resources.GetResourceNameByID(Event.ResourceIDs[I]);
  end;
end;

procedure TcxSchedulerICalendarEventResourcesProperty.SetValue(const AValue: Variant);

  function FindResourceID(const AName: string): Variant;
  var
    I: Integer;
  begin
    Result := Null;
    for I := 0 to ICalendar.Storage.ResourceCount - 1 do
      if CompareText(AName, ICalendar.Storage.Resources.GetResourceNameByID(ICalendar.Storage.ResourceIDs[I])) = 0 then
      begin
        Result := ICalendar.Storage.ResourceIDs[I];
        Break;
      end;
  end;

var
  AStrings: TStrings;
  I: Integer;
  AResourceID: Variant;
begin
  AStrings := StrSplit(cxSchedulerICalendarResourceDelimiter, AValue);
  try
    for I := 0 to AStrings.Count - 1 do
    begin
      AResourceID := FindResourceID(AStrings[I]);
      if AResourceID <> Null then
        Event.ShareWithResource(AResourceID);
    end;
  finally
    AStrings.Free;
  end;
end;

{ TcxSchedulerICalendarTimeZoneStandardRule }

class function TcxSchedulerICalendarTimeZoneStandardRule.TokenName: string;
begin
  Result := 'STANDARD';
end;

{ TcxSchedulerICalendarTimeZoneDaylightRule }

class function TcxSchedulerICalendarTimeZoneDaylightRule.TokenName: string;
begin
  Result := 'DAYLIGHT';
end;

function TcxSchedulerICalendarTimeZoneDaylightRule.IsStandardRule: Boolean;
begin
  Result := False;
end;

{ TcxSchedulerICalendarAlarmCustomProperty }

function TcxSchedulerICalendarAlarmCustomProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := Parent.Parent.Event;
end;

function TcxSchedulerICalendarAlarmCustomProperty.GetParent: TcxSchedulerICalendarAlarmObject;
begin
  Result := TcxSchedulerICalendarAlarmObject(inherited Parent);
end;

{ TcxSchedulerICalendarAlarmTriggerParameter }

class function TcxSchedulerICalendarAlarmTriggerParameter.FromString(
  AProperty: TcxSchedulerICalendarCustomProperty; const ATranslateValue: string; out AValue: Variant): Boolean;
var
  S: string;
  ATriggerValue: Integer;
begin
  S := ATranslateValue;
  if S[1] = '-' then
    Delete(S, 2, 2)
  else
    Delete(S, 1, 2);
  Delete(S, Length(S), 1);
  Result := TryStrToInt(S, ATriggerValue);
  if Result then
    AValue := -ATriggerValue;
end;

class function TcxSchedulerICalendarAlarmTriggerParameter.ToString(
  AProperty: TcxSchedulerICalendarCustomProperty; AValue: Variant; out ATranslateValue: string): Boolean;
const
  FormatMap: array[Boolean] of string = (cxSchedulerICalendarBeforeAlarmTriggerFormat, cxSchedulerICalendarAfterAlarmTriggerFormat);
begin
  ATranslateValue := Format(FormatMap[AValue < 0], [Abs(Integer(AValue))]);
  Result := True;
end;

{ TcxSchedulerICalendarAlarmTriggerProperty }

class function TcxSchedulerICalendarAlarmTriggerProperty.TokenName: string;
begin
  Result := 'TRIGGER';
end;

procedure TcxSchedulerICalendarAlarmTriggerProperty.RegisterCustomParameters;
begin
  inherited;
  AddCustomParameter(TcxSchedulerICalendarAlarmTriggerParameter);
end;

function TcxSchedulerICalendarAlarmTriggerProperty.GetValue: Variant;
begin
  Result := Event.ReminderMinutesBeforeStart;
end;

procedure TcxSchedulerICalendarAlarmTriggerProperty.SetValue(const AValue: Variant);
begin
  Event.ReminderMinutesBeforeStart := AValue;
  Event.Reminder := True;
end;

{ TcxSchedulerICalendarAlarmActionProperty }

class function TcxSchedulerICalendarAlarmActionProperty.TokenName: string;
begin
  Result := 'ACTION';
end;

function TcxSchedulerICalendarAlarmActionProperty.GetValue: Variant;
begin
  Result := 'DISPLAY';
end;

{ TcxSchedulerICalendarAlarmObject }

class function TcxSchedulerICalendarAlarmObject.TokenName: string;
begin
  Result := 'VALARM';
end;

function TcxSchedulerICalendarAlarmObject.CanExport: Boolean;
begin
  Result := inherited CanExport and Parent.Event.Reminder;
end;

procedure TcxSchedulerICalendarAlarmObject.PopulateProperties;
begin
  inherited;
  AddProperty(TcxSchedulerICalendarAlarmTriggerProperty);
  AddProperty(TcxSchedulerICalendarAlarmActionProperty);
end;

function TcxSchedulerICalendarAlarmObject.GetParent: TcxSchedulerICalendarEventObject;
begin
  Result := TcxSchedulerICalendarEventObject(inherited Parent);
end;

{ TcxSchedulerICalendarEventObject }

class function TcxSchedulerICalendarEventObject.TokenName: string;
begin
  Result := 'VEVENT';
end;

class function TcxSchedulerICalendarEventObject.SchedulerObjectClass: TClass;
begin
  Result := TcxSchedulerEvent;
end;

procedure TcxSchedulerICalendarEventObject.AfterImport(AStrings: TStringList);
var
  AInfo: TcxSchedulerEventRecurrenceInfo;
begin
  if Event.IsRecurring then
  begin
    AInfo := Event.RecurrenceInfo;
    if (AInfo.Recurrence = cxreDaily) and (AInfo.Periodicity > 0) then
      AInfo.DayType := cxdtEveryDay;

    if AInfo.GetValidStatus = rvsValid then
      AInfo.Validate
    else
      Event.RemoveRecurrence;
  end;
  Event.Post;
  inherited;
end;

procedure TcxSchedulerICalendarEventObject.DoCreateSchedulerObject;
begin
  FSchedulerObject := Parent.Storage.createEvent;
end;

procedure TcxSchedulerICalendarEventObject.PopulateProperties;
begin
  inherited;
  AddProperty(TcxSchedulerICalendarEventStampProperty);
  AddProperty(TcxSchedulerICalendarEventStartProperty);
  AddProperty(TcxSchedulerICalendarEventEndProperty);
  AddProperty(TcxSchedulerICalendarEventDescriptionProperty);
  AddProperty(TcxSchedulerICalendarEventLocationProperty);
  AddProperty(TcxSchedulerICalendarEventSummaryProperty);
  AddProperty(TcxSchedulerICalendarEventStatusProperty);
  AddProperty(TcxSchedulerICalendarEventUIDProperty);
  AddProperty(TcxSchedulerICalendarEventRecurrenceProperty);
  AddProperty(TcxSchedulerICalendarEventResourcesProperty);
  AddProperty(TcxSchedulerICalendarEventLabelProperty);
  AddProperty(TcxSchedulerICalendarAlarmObject);
end;

function TcxSchedulerICalendarEventObject.GetEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(SchedulerObject);
end;

function TcxSchedulerICalendarEventObject.GetIsAllDayEvent: Boolean;
begin
  Result := Event.AllDayEvent;
end;

procedure TcxSchedulerICalendarEventObject.SetIsAllDayEvent(Value: Boolean);
begin
  Event.AllDayEvent := Value;
end;

function TcxSchedulerICalendarEventObject.GetParent: TcxSchedulerICalendarStorageObject;
begin
  Result := TcxSchedulerICalendarStorageObject(inherited Parent);
end;

{ TcxSchedulerICalendarVersionProperty }

class function TcxSchedulerICalendarVersionProperty.TokenName: string;
begin
  Result := 'VERSION';
end;

function TcxSchedulerICalendarVersionProperty.GetValue: Variant;
begin
  Result := cxSchedulerICalendarExportVersion;
end;

{ TcxSchedulerICalendarProdIDProperty }

class function TcxSchedulerICalendarProdIDProperty.TokenName: string;
begin
  Result := 'PRODID';
end;

function TcxSchedulerICalendarProdIDProperty.GetValue: Variant;
begin
  Result := cxSchedulerICalendarProdID;
end;

{ TcxSchedulerICalendarStorageObject }

class function TcxSchedulerICalendarStorageObject.TokenName: string;
begin
  Result := 'VCALENDAR';
end;

class function TcxSchedulerICalendarStorageObject.SchedulerObjectClass: TClass;
begin
  Result := TcxCustomSchedulerStorage;
end;

procedure TcxSchedulerICalendarStorageObject.DoExport(AStrings: TStringList);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Storage.EventCount - 1 do
    ICalendar.ExportObject(Storage.Events[I], Self, AStrings);
end;

function TcxSchedulerICalendarStorageObject.CanHaveProperty(AProperty: TcxSchedulerICalendarCustomPropertyClass): Boolean;
begin
  Result := inherited CanHaveProperty(AProperty) or (AProperty = TcxSchedulerICalendarEventObject);
end;

procedure TcxSchedulerICalendarStorageObject.PopulateProperties;
begin
  inherited;
  AddProperty(TcxSchedulerICalendarVersionProperty);
  AddProperty(TcxSchedulerICalendarProdIDProperty);
  AddProperty(TcxSchedulerICalendarTimeZoneObject);
end;

procedure TcxSchedulerICalendarStorageObject.DoCreateSchedulerObject;
begin
  FSchedulerObject := ICalendar.Storage;
end;

function TcxSchedulerICalendarStorageObject.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := TcxCustomSchedulerStorage(FSchedulerObject);
end;

{ TcxSchedulerICalendar }

constructor TcxSchedulerICalendar.Create(AStorage: TcxCustomSchedulerStorage);
begin
  inherited Create;
  FStorage := AStorage;
  FTimeZone := GetStorageTimeZone;
end;

procedure TcxSchedulerICalendar.Export(const AFileName: string);
var
  AStrings: TStringList;
begin
  FDateTime := Now;
  AStrings := TStringList.Create;
  try
    ExportObject(Storage, nil, AStrings);
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

procedure TcxSchedulerICalendar.Import(const AFileName: string);
var
  AStrings: TStringList;
  I: Integer;
  S: string;
begin
  AStrings := TStringList.Create;
  try
    AStrings.LoadFromFile(AFileName);
    for I := AStrings.Count - 1 downto 1 do
    begin
      S := AStrings[I];
      if (Length(S) > 0) and (Ord(S[1]) in [VK_TAB, VK_SPACE]) then
      begin
        while (Length(S) > 0) and (Ord(S[1]) in [VK_TAB, VK_SPACE]) do
          Delete(S, 1, 1);
        AStrings[I - 1] := AStrings[I - 1] + S;
        AStrings.Delete(I);
      end;
    end;
    while (AStrings.Count > 0)  do
      ImportObject(nil, AStrings);
  finally
    AStrings.Free;
  end;
end;

function TcxSchedulerICalendar.CanExport(AObject: TcxSchedulerICalendarCustomObject): Boolean;
begin
  Result := True;
end;

function TcxSchedulerICalendar.CanImport(AObject: TcxSchedulerICalendarCustomObject): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerICalendar.ExportObject(AObject: TObject; AParent: TcxSchedulerICalendarCustomObject; AStrings: TStringList);
var
  AICalendarProperty: TcxSchedulerICalendarCustomProperty;
begin
  AICalendarProperty := cxSchedulerICalendarPropertyManager.GetICalendarPropertyClass(AObject).Create(Self, AParent);
  try
    AICalendarProperty.FSchedulerObject := AObject;
    AICalendarProperty.Export(AStrings);
  finally
    AICalendarProperty.Free;
  end;
end;

procedure TcxSchedulerICalendar.ImportObject(AParent: TcxSchedulerICalendarCustomObject; AStrings: TStringList);
var
  AImportPropertyClass: TcxSchedulerICalendarCustomPropertyClass;
  AImportProperty: TcxSchedulerICalendarCustomProperty;
begin
  AImportPropertyClass := cxSchedulerICalendarPropertyManager.GetICalendarPropertyClass(AStrings[0], AParent);
  if AImportPropertyClass = nil then
    AStrings.Delete(0)
  else
  begin
    AImportProperty := AImportPropertyClass.Create(Self, AParent);
    try
      AImportProperty.Import(AStrings);
    finally
      AImportProperty.Free;
    end;
  end;
end;

function TcxSchedulerICalendar.FindTimeZone(const AName: string): Integer;
begin
  for Result := 0 to TcxSchedulerDateTimeHelper.TimeZoneCount - 1 do
    if (CompareText(AName, TcxSchedulerDateTimeHelper.TimeZoneInfo(Result).Display) = 0) or
      (CompareText(AName, TcxSchedulerDateTimeHelper.TimeZoneInfo(Result).StandardName) = 0) or
      (CompareText(AName, TcxSchedulerDateTimeHelper.TimeZoneInfo(Result).DaylightName) = 0) then
        Exit;
  Result := -1;
end;

function TcxSchedulerICalendar.GetStorageTimeZone: Integer;
begin
  Result := TcxSchedulerDateTimeHelper.CurrentTimeZone;
end;

procedure RegisterICalendarObjects;
begin
  TcxSchedulerICalendarStorageObject.Register;

  TcxSchedulerICalendarVersionProperty.Register;
  TcxSchedulerICalendarProdIDProperty.Register;

  TcxSchedulerICalendarTimeZoneObject.Register;
  TcxSchedulerICalendarTZIDProperty.Register;
  TcxSchedulerICalendarTimeZoneStandardRule.Register;
  TcxSchedulerICalendarTimeZoneDaylightRule.Register;
  TcxSchedulerICalendarTimeZoneRuleRecurrence.Register;
  TcxSchedulerICalendarTZOffsetFromProperty.Register;
  TcxSchedulerICalendarTZOffsetToProperty.Register;
  TcxSchedulerICalendarTimeZoneStartProperty.Register;

  TcxSchedulerICalendarEventObject.Register;
  TcxSchedulerICalendarEventLabelProperty.Register;
  TcxSchedulerICalendarEventResourcesProperty.Register;
  TcxSchedulerICalendarAlarmObject.Register;
  TcxSchedulerICalendarAlarmTriggerProperty.Register;
  TcxSchedulerICalendarAlarmActionProperty.Register;
  TcxSchedulerICalendarEventStampProperty.Register;
  TcxSchedulerICalendarEventStartProperty.Register;
  TcxSchedulerICalendarEventEndProperty.Register;
  TcxSchedulerICalendarEventDescriptionProperty.Register;
  TcxSchedulerICalendarEventLocationProperty.Register;
  TcxSchedulerICalendarEventSummaryProperty.Register;
  TcxSchedulerICalendarEventStatusProperty.Register;
  TcxSchedulerICalendarEventRecurrenceProperty.Register;
  TcxSchedulerICalendarEventUIDProperty.Register;
end;

procedure UnregisterICalendarObjects;
begin
  TcxSchedulerICalendarEventObject.Unregister;
  TcxSchedulerICalendarEventLabelProperty.Unregister;
  TcxSchedulerICalendarEventResourcesProperty.Unregister;
  TcxSchedulerICalendarAlarmObject.Unregister;
  TcxSchedulerICalendarAlarmTriggerProperty.Unregister;
  TcxSchedulerICalendarAlarmActionProperty.Unregister;
  TcxSchedulerICalendarEventStampProperty.Unregister;
  TcxSchedulerICalendarEventStartProperty.Unregister;
  TcxSchedulerICalendarEventEndProperty.Unregister;
  TcxSchedulerICalendarEventDescriptionProperty.Unregister;
  TcxSchedulerICalendarEventLocationProperty.Unregister;
  TcxSchedulerICalendarEventSummaryProperty.Unregister;
  TcxSchedulerICalendarEventStatusProperty.Unregister;
  TcxSchedulerICalendarEventRecurrenceProperty.Unregister;
  TcxSchedulerICalendarEventUIDProperty.Unregister;

  TcxSchedulerICalendarTimeZoneObject.Unregister;
  TcxSchedulerICalendarTimeZoneStartProperty.Unregister;
  TcxSchedulerICalendarTZOffsetToProperty.Unregister;
  TcxSchedulerICalendarTZOffsetFromProperty.Unregister;
  TcxSchedulerICalendarTimeZoneRuleRecurrence.Unregister;
  TcxSchedulerICalendarTimeZoneDaylightRule.Unregister;
  TcxSchedulerICalendarTimeZoneStandardRule.Unregister;
  TcxSchedulerICalendarTZIDProperty.Unregister;

  TcxSchedulerICalendarProdIDProperty.Unregister;
  TcxSchedulerICalendarVersionProperty.Unregister;

  TcxSchedulerICalendarStorageObject.Unregister;
end;

procedure CalculateProdID;
var
  AInfo: TcxVersionInfo;
begin
  AInfo := TcxVersionInfo.Create;
  try
    cxSchedulerICalendarProdID := '-//' + AInfo.CompanyName + '//' + AInfo.ProductName + '//' +
      AInfo.Language;
  finally
    AInfo.Free;
  end;
end;

initialization
  RegisterICalendarObjects;
  CalculateProdID;

finalization
  UnregisterICalendarObjects;
  FreeAndNil(FObjectManager);

end.
