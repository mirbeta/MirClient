//---------------------------------------------------------------------------

#ifndef uSampleGaugesH
#define uSampleGaugesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxGaugeCircularScale.hpp"
#include "dxGaugeControl.hpp"
#include "dxGaugeCustomScale.hpp"
#include "dxGaugeDigitalScale.hpp"
#include "dxGaugeLinearScale.hpp"
#include "dxGaugeQuantitativeScale.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmSampleGauges : public TForm
{
__published:	// IDE-managed Components
	TPageControl *PageControl1;
	TTabSheet *tsCircularGauges;
	TdxGaugeControl *gcCircularWhite;
	TdxGaugeCircularScale *dxGaugeCircularScale4;
	TdxGaugeQuantitativeScaleCaption *dxGaugeCircularScale4Caption1;
	TdxGaugeQuantitativeScaleCaption *dxGaugeCircularScale4Caption2;
	TdxGaugeCircularScaleRange *dxGaugeCircularScale4Range;
	TdxGaugeCircularScaleRange *dxGaugeCircularScale4Range1;
	TdxGaugeCircularScaleRange *dxGaugeCircularScale4Range2;
	TdxGaugeControl *gcCircularDeepFire;
	TdxGaugeCircularScale *dxGaugeCircularScale3;
	TdxGaugeQuantitativeScaleCaption *dxGaugeCircularScale3Caption1;
	TdxGaugeQuantitativeScaleCaption *dxGaugeCircularScale3Caption2;
	TdxGaugeCircularScale *dxGaugeControl6dxGaugeCircularScale1;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl6dxGaugeCircularScale1Caption1;
	TdxGaugeControl *gcCircularCleanWhite;
	TdxGaugeCircularScale *dxGaugeCircularScale1;
	TTabSheet *tsCircularHalfGauges;
	TdxGaugeControl *gcCircularHalfYellowSubmarine;
	TdxGaugeCircularHalfScale *dxGaugeControl4CircularHalfScale1;
	TdxGaugeCircularHalfScale *dxGaugeControl4CircularHalfScale2;
	TdxGaugeControl *gcCircularHalfClassic;
	TdxGaugeCircularHalfScale *dxGaugeCircularHalfScale1;
	TdxGaugeControl *gcCircularHalfCleanWhite;
	TdxGaugeCircularHalfScale *dxGaugeCircularHalfScale2;
	TdxGaugeCircularScaleRange *dxGaugeCircularHalfScale2Range;
	TdxGaugeCircularScaleRange *dxGaugeCircularHalfScale2Range1;
	TdxGaugeCircularScaleRange *dxGaugeCircularHalfScale2Range2;
	TTabSheet *tsCircularQuarterGauges;
	TdxGaugeControl *gcCircularQuarterYellowSubmarine;
	TdxGaugeCircularQuarterLeftScale *dxGaugeControl7CircularQuarterLeftScale1;
	TdxGaugeControl *gcCircularQuarterDeepFire;
	TdxGaugeCircularQuarterRightScale *dxGaugeControl8CircularQuarterRightScale1;
	TdxGaugeCircularScaleRange *dxGaugeControl8CircularQuarterRightScale1Range;
	TdxGaugeCircularScaleRange *dxGaugeControl8CircularQuarterRightScale1Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl8CircularQuarterRightScale1Range2;
	TdxGaugeControl *gcCircularQuarterSmart;
	TdxGaugeCircularQuarterRightScale *dxGaugeControl9CircularQuarterRightScale1;
	TTabSheet *tsCircularThreeFourth;
	TdxGaugeControl *gcCircularThreeFourthAfrica;
	TdxGaugeCircularThreeFourthScale *dxGaugeControl5CircularThreeFourthScale1;
	TdxGaugeControl *gcCircularThreeFourthFuture;
	TdxGaugeCircularThreeFourthScale *dxGaugeControl6CircularThreeFourthScale1;
	TdxGaugeControl *gcCircularThreeFourthDisco;
	TdxGaugeCircularThreeFourthScale *dxGaugeControl7CircularThreeFourthScale1;
	TTabSheet *tsCircularWide;
	TdxGaugeControl *gcCircularWideMechanical;
	TdxGaugeCircularWideScale *Scale3;
	TdxGaugeControl *gcCircularWideWhite;
	TdxGaugeCircularWideScale *Scale2;
	TdxGaugeControl *gcCircularWideDeepFire;
	TdxGaugeCircularWideScale *Scale1;
	TdxGaugeControl *gcCircularWideSportCar;
	TdxGaugeCircularWideScale *dxGaugeCircularWideScale1;
	TdxGaugeControl *gcCircularWideAfrica;
	TdxGaugeCircularWideScale *dxGaugeCircularWideScale2;
	TdxGaugeControl *gcCircularWideDarkNight;
	TdxGaugeCircularWideScale *dxGaugeCircularWideScale3;
	TTabSheet *tsLinearGauges;
	TdxGaugeControl *gcLinearSmart;
	TdxGaugeLinearScale *dxGaugeLinearScale1;
	TdxGaugeLinearScale *dxGaugeControl2LinearScale1;
	TdxGaugeLinearScaleRange *dxGaugeControl2LinearScale1Range;
	TdxGaugeLinearScaleRange *dxGaugeControl2LinearScale1Range1;
	TdxGaugeControl *gcLinearYellowSubmarine;
	TdxGaugeLinearScale *dxGaugeLinearScale2;
	TdxGaugeLinearScale *dxGaugeControl3LinearScale1;
	TdxGaugeControl *gcLinearIceColdZone;
	TdxGaugeLinearScale *dxGaugeControl1LinearScale1;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl1LinearScale1Caption1;
	TTabSheet *tsDigital;
	TdxGaugeControl *gcDigitalIceColdZone;
	TdxGaugeDigitalScale *dxGaugeDigitalScale3;
	TdxGaugeControl *gcDigitalDeepFire;
	TdxGaugeDigitalScale *dxGaugeDigitalScale5;
	TdxGaugeControl *gcDigitalWhite;
	TdxGaugeDigitalScale *dxGaugeDigitalScale6;
	TdxGaugeControl *gcDigitalScaleText;
	TdxGaugeDigitalScale *dxGaugeDigitalScale7;
	TdxGaugeControl *gcDigitalClassic;
	TdxGaugeDigitalScale *dxGaugeDigitalScale1;
	TdxGaugeControl *gcDigitalFuture;
	TdxGaugeDigitalScale *dxGaugeDigitalScale8;
	TTabSheet *tsHybridGauges;
	TPageControl *pcHybridGauges;
	TTabSheet *tsCarTester;
	TdxGaugeControl *gcTester;
	TdxGaugeCircularScale *gsMainScaleBackground;
	TdxGaugeCircularScale *gsColoredScaleBackground;
	TdxGaugeCircularScale *gsTesterValue4;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale3Range;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale3Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale3Range2;
	TdxGaugeDigitalScale *gsTesterValue5;
	TdxGaugeDigitalScale *gsTesterValue6;
	TdxGaugeDigitalScale *gsTesterValue7;
	TdxGaugeCircularScale *gsTesterValue3;
	TdxGaugeQuantitativeScaleCaption *gsTesterValue3Caption1;
	TdxGaugeCircularScale *gsScaleTicks2;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale5Range;
	TdxGaugeCircularScale *gsScaleTicks1;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale6Range;
	TdxGaugeLinearScale *gsMainBackground;
	TdxGaugeDigitalScale *gsBackground;
	TdxGaugeCircularScale *gsTesterValue1;
	TdxGaugeQuantitativeScaleCaption *gsTesterValue1Caption1;
	TdxGaugeQuantitativeScaleCaption *gsTesterValue1Caption2;
	TdxGaugeQuantitativeScaleCaption *gsTesterValue1Caption3;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale7Range;
	TdxGaugeCircularScale *gsTesterValue2;
	TTabSheet *tsTemperatureGauges;
	TdxGaugeControl *dxGaugeControl1;
	TdxGaugeCircularScale *gsPressureValue;
	TdxGaugeQuantitativeScaleCaption *gsPressureValueCaption1;
	TdxGaugeQuantitativeScaleCaption *gsPressureValueCaption2;
	TdxGaugeCircularScaleRange *gsPressureValueRange1;
	TdxGaugeCircularScaleRange *gsPressureValueRange2;
	TdxGaugeLinearScale *gsVolumeValue;
	TdxGaugeQuantitativeScaleCaption *gsVolumeValueCaption1;
	TdxGaugeLinearScale *gsVolumeValueSecondary;
	TdxGaugeQuantitativeScaleCaption *gsVolumeValueSecondaryCaption1;
	TdxGaugeLinearScaleRange *dxGaugeControl1LinearScale2Range;
	TdxGaugeLinearScaleRange *dxGaugeControl1LinearScale2Range1;
	TdxGaugeLinearScaleRange *dxGaugeControl1LinearScale2Range2;
	TdxGaugeContainerScale *dxGaugeControl1ContainerScale1;
	TTabSheet *TabSheet1;
	TdxGaugeControl *dxGaugeControl2;
	TdxGaugeCircularScale *gcsHazeGas;
	TdxGaugeQuantitativeScaleCaption *gcsHazeGasCaptionValue;
	TdxGaugeQuantitativeScaleCaption *gcsHazeCaption1;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale1Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale1Range2;
	TdxGaugeContainerScale *dxGaugeControl2ContainerScale1;
	TdxGaugeCircularScale *gcsHazeColdWater;
	TdxGaugeQuantitativeScaleCaption *gcsHazeColdWaterCaptionValue;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl2CircularScale1Caption3;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale1Range3;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale1Range4;
	TdxGaugeCircularScale *gcsHazeHotWater;
	TdxGaugeQuantitativeScaleCaption *gcsHazeHotWaterCaptionValue;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl2CircularScale2Caption2;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale2Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale2Range2;
	TdxGaugeCircularScale *gcsHazeElectricity;
	TdxGaugeQuantitativeScaleCaption *gcsHazeElectricityCaptionValue;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl2CircularScale3Caption2;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale3Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl2CircularScale3Range2;
	TTabSheet *tsWeather;
	TdxGaugeControl *gcWeather;
	TdxGaugeContainerScale *dxGaugeControl3ContainerScale1;
	TdxGaugeCircularScale *gcsWeatherLosAnglesHumidity;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherLosAnglesHumidityCaption;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale1Range1;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale1Range2;
	TdxGaugeCircularScale *gcsWeatherLosAnglesTemperature;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherLosAnglesTemperatureCaption;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl3CircularScale3Caption3;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale3Range3;
	TdxGaugeCircularScaleRange *gcsWeatherLosAnglesTemperatureRange;
	TdxGaugeCircularScale *gcsWeatherMoscowHumidity;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherMoscowHumidityCaption;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale4Range1;
	TdxGaugeCircularScaleRange *gcsWeatherMowcowHumidityRange;
	TdxGaugeCircularScale *gcsWeatherMoscowTemperature;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherMoscowTemperatureCaption;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl3CircularScale2Caption3;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherDate;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale2Range1;
	TdxGaugeCircularScaleRange *gcsWeatherMoscowTemperatureRange;
	TdxGaugeCircularScale *gcsWeatherLondonHumidity;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherLondonHumidityCaption;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale5Range1;
	TdxGaugeCircularScaleRange *gcsWeatherLondonHumidityRange;
	TdxGaugeCircularScale *gcsWeatherLondonTemperature;
	TdxGaugeQuantitativeScaleCaption *gcsWeatherLondonTemperatureCaption;
	TdxGaugeQuantitativeScaleCaption *dxGaugeControl3CircularScale6Caption3;
	TdxGaugeCircularScaleRange *dxGaugeControl3CircularScale6Range1;
	TdxGaugeCircularScaleRange *gcsWeatherLondonTemperatureRange;
	TTabSheet *tsSimpleHybrid;
	TdxGaugeControl *gcHybridDarkNight;
	TdxGaugeCircularScale *dxGaugeCircularScale2;
	TdxGaugeDigitalScale *dxGaugeDigitalScale4;
	TdxGaugeControl *gcHybrid;
	TdxGaugeCircularScale *dxGaugeCircularScale5;
	TdxGaugeDigitalScale *dxGaugeControl11dxGaugeDigitalScale1;
	TdxGaugeContainerScale *gcHybridContainerScale1;
	TdxGaugeLinearScale *gcHybridLinearScale1;
	TdxGaugeQuantitativeScaleCaption *gcHybridLinearScale1Caption1;
	TdxGaugeCircularWideScale *gcHybridCircularWideScale1;
	TdxGaugeControl *gcHybridIceColdZone;
	TdxGaugeCircularScale *dxGaugeControl3dxGaugeCircularScale1;
	TdxGaugeDigitalScale *dxGaugeDigitalScale2;
	TdxGaugeCircularScale *gcHybridIceColdZoneCircularScale1;
	TdxGaugeQuantitativeScaleCaption *gcHybridIceColdZoneCircularScale1Caption1;
	TcxLabel *lbDescription;
	TTimer *tCarTesterTimer;
	TTimer *tTemperatureTimer;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TTimer *tHazesTimer;
	TTimer *tWeatherTimer;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall PageControl1Change(TObject *Sender);
	void __fastcall PageControl1Resize(TObject *Sender);
	void __fastcall pcHybridGaugesChange(TObject *Sender);
	void __fastcall tCarTesterTimerTimer(TObject *Sender);
	void __fastcall tHazesTimerTimer(TObject *Sender);
	void __fastcall tTemperatureTimerTimer(TObject *Sender);
	void __fastcall tWeatherTimerTimer(TObject *Sender);
	void __fastcall gcsWeatherLosAnglesTemperatureAnimate(TObject *Sender);
	void __fastcall gcsWeatherLosAnglesHumidityAnimate(TObject *Sender);
	void __fastcall gcsWeatherMoscowHumidityAnimate(TObject *Sender);
	void __fastcall gcsWeatherLondonHumidityAnimate(TObject *Sender);
	void __fastcall gcsWeatherMoscowTemperatureAnimate(TObject *Sender);
	void __fastcall gcsWeatherLondonTemperatureAnimate(TObject *Sender);
private:	// User declarations
	TDateTime FDate;
	double GetRandomValueDelta(int AFrom, int ATo, int ARange);
	double GetRandomValue(int AFrom, int ATo, int ARange);
	void UpdateWeatherDate();
	void UpdateGauges();
	void UpdateCircularGauges();
	void UpdateCircularHalfGauges();
	void UpdateCircularQuarterGauges();
	void UpdateCircularThreeFouthGauges();
	void UpdateCircularWideGauges();
	void UpdateLinearGauges();
	void UpdateHybridGauges();
	void UpdateDigitalGauges();
	void UpdateTimers();
	void LoadCustomStyles();
	void UpdateLosAngeles();
	void UpdateLondonWeather();
	void UpdateMoscowWeather();
	void UpdateSityWeather(TdxGaugeCircularScale * ATemperatureScale, TdxGaugeCircularScale* AHumidityScale,
	  Single ATemperature, Single AHumidity);
	void UpdateSityTemperature(Single AValue, TdxGaugeQuantitativeScaleCaption * ACaption,
	  TdxGaugeCircularScaleRange * ARange);
public:		// User declarations
	__fastcall TfrmSampleGauges(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSampleGauges *frmSampleGauges;
//---------------------------------------------------------------------------
#endif
