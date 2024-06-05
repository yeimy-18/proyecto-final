program sistema_particulas_cargadas
    implicit none

    ! Definir constantes y parámetros
    real, parameter :: K = 1.0
    integer, parameter :: N = 100
    real, parameter :: L = 10.0

    ! Definir estructura para una partícula
    type :: Particle
        real :: x, y
        real :: charge
    end type Particle

    ! Declarar variables
    type(Particle) :: particles(N)
    real :: total_energy
    integer :: i, j

    ! Inicializar posiciones y cargas de las partículas
    do i = 1, N
        particles(i)%x = L * rand()
        particles(i)%y = L * rand()
        particles(i)%charge = 1.0 ! Carga de ejemplo
    end do

    ! Calcular la energía total del sistema
    total_energy = 0.0
    do i = 1, N
        do j = i+1, N
            total_energy = total_energy + coulomb_interaction(particles(i), particles(j))
        end do
    end do

    print *, "Energía total del sistema:", total_energy

contains

    ! Subrutina para calcular la interacción de Coulomb entre dos partículas
    real function coulomb_interaction(p1, p2)
        type(Particle), intent(in) :: p1, p2
        real :: rij

        rij = sqrt((p1%x - p2%x)**2 + (p1%y - p2%y)**2)
        coulomb_interaction = K * p1%charge * p2%charge / rij
    end function coulomb_interaction

end program sistema_particulas_cargadas