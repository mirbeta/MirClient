//---------------------------------------------------------------------------

#ifndef uCarDashboardH
#define uCarDashboardH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxGaugeCircularScale.hpp"
#include "dxGaugeControl.hpp"
#include "dxGaugeCustomScale.hpp"
#include "dxGaugeDigitalScale.hpp"
#include "dxGaugeQuantitativeScale.hpp"
#include "dxGDIPlusClasses.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "dxSkinMetropolisDark.hpp"
#include "dxSkinsCore.hpp"
#include "dxSkinsForm.hpp"

//---------------------------------------------------------------------------
class TfrmCarDashboard : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TdxGaugeControl *dxGaugeControl1;
	TdxGaugeCircularScale *gsBackground;
	TdxGaugeCircularScale *gsCentralPanel;
	TdxGaugeCircularScale *gsSpeedometr;
	TdxGaugeQuantitativeScaleCaption *gsSpeedometrCaption1;
	TdxGaugeQuantitativeScaleCaption *gsSpeedometrCaption2;
	TdxGaugeCircularScale *gsTachometer;
	TdxGaugeQuantitativeScaleCaption *gsTachometerCaption1;
	TdxGaugeCircularScaleRange *gsTachometerRange;
	TdxGaugeCircularScale *gsTemperature;
	TdxGaugeDigitalScale *gsGear;
	TdxGaugeDigitalScale *gsFuelIndicator;
	TdxGaugeDigitalScale *gsCarStateIndicator;
	TdxGaugeCircularHalfScale *gsFuel;
	TdxGaugeQuantitativeScaleCaption *gsFuelCaption1;
	TdxGaugeQuantitativeScaleCaption *gsFuelCaption2;
	TcxButton *btnDownGear;
	TcxButton *btnUpGear;
	TcxButton *btnNeutralGear;
	TcxButton *btnEngineStop;
	TcxButton *btnEngineStart;
	TcxButton *btnAccelerate;
	TcxButton *btnFueling;
	TcxButton *btnBrake;
	TdxGaugeControl *dxGaugeControl2;
	TdxGaugeDigitalScale *gsDescription;
	TTimer *Timer1;
	TTimer *tDigitalScaleTimer;
	TcxLookAndFeelController *cxLookAndFeelController1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall tDigitalScaleTimerTimer(TObject *Sender);
	void __fastcall btnEngineStartClick(TObject *Sender);
	void __fastcall btnEngineStopClick(TObject *Sender);
	void __fastcall btnFuelingClick(TObject *Sender);
	void __fastcall btnBrakeMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall btnBrakeMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall btnAccelerateMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall btnAccelerateMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall btnUpGearClick(TObject *Sender);
	void __fastcall btnNeutralGearClick(TObject *Sender);
	void __fastcall btnDownGearClick(TObject *Sender);
private:	// User declarations
    Integer FIndex;
	String FDescriptionText;

	Integer FGear;
	Single FGearFactors[5];

	Boolean FStarted;
	Boolean FIsStarting;
	Boolean FIsBreaking;

	Single FFuelValueDelta;
	Single FTachometerValueDelta;
	Single FTemperatureValueDelta;
	TdxSkinController* FSkinController;
	void Stop();
	void UpdateGear();
	void UpdateTachometer();
	void UpdateSpeed();
	void UpdateFuelLevel();
	void UpdateTemperature();
	void UpdateIndicators();
	void UpdateButtonStates();
	void PrepareDesription();
	void Start();
	void SetGear(int ADelta);
	void BreakDown();
	void GearUp();
	void GearDown();
	void BreakUp();
	Single GetTachometerValue();
	void AccelerateUp();
	void AccelerateDown();
public:		// User declarations
	__fastcall TfrmCarDashboard(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCarDashboard *frmCarDashboard;
//---------------------------------------------------------------------------
#endif
