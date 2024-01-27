subroutine voltage_regulator(var_array, Vout) 
        
    real, dimension(1:5), intent(in) :: var_array
    real, intent(out) :: Vout
    real :: Vref, Iadj, R3, R2, R1
        
        Vref = var_array(5)
        Iadj = var_array(4)
        R3   = var_array(3)
        R2   = var_array(2)
        R1   = var_array(1)

        Vout = Vref * (1 + (R1 / (R2 + R3))) + (Iadj * (R2 + R3))
            
end subroutine voltage_regulator