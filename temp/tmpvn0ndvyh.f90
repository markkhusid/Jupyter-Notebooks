program dijkstra
    implicit none
    integer, parameter :: n = 4
    integer :: i, j, start_vertex
    integer :: graph(n, n)
    integer :: distances(n), visited(n)
    integer :: current_vertex, min_distance, next_vertex

    ! Initialize graph with adjacency matrix
    ! Use 9999 to represent infinity
    graph = reshape((/ 0,    1,    9999, 9999, &
                       1,    0,    2   , 9999, &
                       9999, 2,    0,    1, &
                       9999, 9999, 1,    0 /), shape(graph))

    ! Initialize distances and visited arrays
    distances = 9999
    visited = 0

    ! Set the start vertex
    start_vertex = 1

    ! Distance from start vertex to itself is 0
    distances(start_vertex) = 0

    ! Dijkstra's algorithm
    do i = 1, n
        ! Find the vertex with the minimum distance value
        min_distance = 9999
        do j = 1, n
            if (.not. visited(j) .and. distances(j) < min_distance) then
                min_distance = distances(j)
                current_vertex = j
            end if
        end do

        ! Mark the current vertex as visited
        visited(current_vertex) = 1

        ! Update distances of adjacent vertices
        do j = 1, n
            if (.not. visited(j) .and. graph(current_vertex, j) /= 9999) then
                if (distances(current_vertex) + graph(current_vertex, j) < distances(j)) then
                    distances(j) = distances(current_vertex) + graph(current_vertex, j)
                end if
            end if
        end do
    end do

    ! Print the shortest distances
    print *, "Vertex    Distance from Source"
    do i = 1, n
        print *, i, "          ", distances(i)
    end do

end program dijkstra
