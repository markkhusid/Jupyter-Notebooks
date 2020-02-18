subroutine voltage_divider(Vin, r_array, Vout) 
        
    real, intent(in) :: Vin
    real, dimension(1:3), intent(in) :: r_array
    real, intent(out) :: Vout
        
        Vout = Vin * (r_array(1) / (r_array(1) + r_array(2) + r_array(3)))
            
end subroutine voltage_divider