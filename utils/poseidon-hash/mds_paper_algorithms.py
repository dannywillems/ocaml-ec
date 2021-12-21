# Algorithms to check the security of an MDS matrix from
# https://eprint.iacr.org/2020/500
# Original implementation in
# https://gitlab.com/dannywillems/linear-layer-tool/-/blob/master/code/algorithms_gfp.sage

# We apply an S-box to the just last element in partial rounds.

from sage.all import *

def subspace_times_matrix(subspace, M, V):
    basis = [M * v for v in subspace.basis()]
    return V.subspace(basis)

def algorithm_1(M, K):
    t = M.ncols()
    R = PolynomialRing(K, "X")
    decomposition = M.minimal_polynomial().squarefree_decomposition()

    V = VectorSpace(K, t)
    unit_vector_space = V.subspace(V.basis()[:-1])

    basis_vectors = []
    for poly, exp in decomposition:
        A_i = R(poly ** exp)(M).right_kernel()
        X_i = A_i.intersection(unit_vector_space)
        while X_i.dimension() > 0:
            X_i_new = X_i.intersection(subspace_times_matrix(X_i, M, V))
            if X_i == X_i_new:
                break
            X_i = X_i_new
        basis_vectors += X_i.basis()
    P_full_space = V.subspace(basis_vectors)
    return P_full_space.dimension() == 0

def algorithm_2(M, K):
    t = M.ncols()
    V = VectorSpace(K, t)

    I = [t - 1]
    I_powerset = list(sage.misc.misc.powerset(I))[1:]
    for I_s in I_powerset:
        test_next = False
        new_basis = []
        for l in I_s:
            new_basis.append(V.basis()[l])
        IS = V.subspace(new_basis)
        for i in range(t - 1):
            new_basis.append(V.basis()[i])
        full_iota_space = V.subspace(new_basis)
        for l in I_s:
            v = V.basis()[l]
            while True:
                delta = IS.dimension()
                v = M * v
                IS = V.subspace(IS.basis() + [v])
                if IS.dimension() == t or IS.intersection(full_iota_space) != IS:
                    test_next = True
                    break
                if IS.dimension() <= delta:
                    break
            if test_next:
                break
        if test_next:
            continue
        return [False, [IS, I_s]]

    return [True, None]

def algorithm_3(M, K):
    t = M.ncols()
    V = VectorSpace(K, t)
    l = 2 * t

    flag_secure = True
    subspaces_found = []

    M_round = []
    for j in range(0, l + 1):
        M_round.append(M ** (j + 1))

    I = [t - 1]
    I_powerset = list(sage.misc.misc.powerset(I))[1:]

    for r in range(2, l + 1):
        next_r = False
        for I_s in I_powerset:
            IS = None
            res_alg_2 = algorithm_2(M ** r, K)
            if res_alg_2[1] == None:
                continue

            (IS, I_s) = res_alg_2[1]

            if IS != None and IS.dimension() > 0:
                active_sbox_positions = [[] for _ in range(0, r)]
                active_sbox_positions[0] = I_s
                for j in range(1, r):
                    if IS == subspace_times_matrix(IS, M):
                        next_r = True
                        break
                    IS = subspace_times_matrix(IS, M)
                    for i in range(0, s):
                        new_basis = []
                        for k in range(0, t):
                            if k != i:
                                new_basis.append(V.basis()[k])
                        iota_space = V.subspace(new_basis)
                        if IS.intersection(iota_space) != IS:
                            single_iota_space = V.subspace([V.basis()[i]])
                            if IS.intersection(single_iota_space) == single_iota_space:
                                active_sbox_positions[j].append(i)
                            else:
                                next_r = True
                                break
                    if next_r:
                        break
                if next_r:
                    break
                if active_sbox_positions != [[] for _ in range(0, r)]:
                    flag_secure = False
                    subspaces_found.append([IS, r, active_sbox_positions])
        if next_r:
            continue

    return flag_secure
